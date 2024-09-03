import numpy as np
import torch
import pandas as pd
import random
import matplotlib.pyplot as plt
import os

from encoder_decoder import seq2seq

def get_config():
        config = {
                "batch_size": 32,
                "epochs": 200, # either 100 or 200; don't go above that
                "learning_rate": 0.0001, # might be good to include? depends on scheduler?
                "eval_freq": 10,
                "batch_shuffle": True,
                "dropout":0.2, # include in grid search dropout_list = [0, 0.0001, 0.0005, 0.001, 0.002, 0.01]
                "num_layers": 1, # keep as 1 num_layers_list = [1, 2, 3] 
                "hidden_feature_size": 16, # keep small (try 8) hidden_feature_size_list = [8, 16] if increase this, need to increase dropout
                "model_type": 'LSTM',
                "teacher_forcing_ratio": 0.0,
                "max_lr": 5e-4,
                "div_factor": 100,
                "pct_start": 0.05,
                "anneal_strategy": 'cos',
                "final_div_factor": 10000.0,
                "dataset": 'LSTM_dataset.csv',
                "split_date":'2021-12-31',
                "input_window":14, # up to 70
                "output_window":7, # up to 35
                "early_stop_thres":5, # not being used
                "early_stop_delta":0.5, # not being used
                "early_stop":False, # this shows that early stop not being used
                "weight_decay":0.05, # 0.001? decay_list = [0, 0.0001, 0.0005, 0.001, 0.002, 0.01] should be 10^-3 or smaller
                "training_prediction":'recursive'
            }        
        return config
    
def run_all(split_date, input_window, output_window, epochs, dropout, num_layers, hidden_feature_size, weight_decay):
    
    if torch.cuda.is_available():
        device = torch.device('cuda')
    else:
        device = torch.device('cpu')

    path = './code/model_files/LSTM'
    file_name = 'LSTM_dataset.csv'
    metadata = 'LSTM_metadata.csv'
    
    seed = 2024

    np.random.seed(seed)

    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.cuda.manual_seed(seed)
    torch.backends.cudnn.benchmark = False
    torch.backends.cudnn.deterministic = True

    # Read metadata file
    dx = pd.read_csv(os.path.join(path, metadata))

    # Extract all col names from Metadata
    feature_cols = dx[dx['column_type']=='feature']['column_names'].tolist()  # feature colums represent the input drivers
    target_cols = dx[dx['column_type']=='target']['column_names'].tolist()   # target column represent the chlorophyll values
    date_col = dx[dx['column_type']=='date']['column_names'].tolist()[0]    # date column stores the date timeline

    df = pd.read_csv(os.path.join(path, file_name))
                     
    feature_cols += target_cols
 
    utils = Utils(num_features=len(feature_cols), inp_cols=feature_cols, target_cols=target_cols, date_col=date_col,
                  num_out_features=1, device=device)
    
    params = {
      'input_window': int(input_window),
      'output_window': int(output_window),
      'split_date': split_date,
      'epochs': int(epochs),
      'dropout': dropout,
      'num_layers': int(num_layers),
      'hidden_feature_size': int(hidden_feature_size),
      'weight_decay': weight_decay,
    }
    
    utils.run_all_fn(df=df,
                    params=params,
                    stride=1)
                     
                     
class Utils:
    
    def __init__(self, num_features, inp_cols, target_cols, date_col, num_out_features, device):
        self.num_features = num_features
        self.inp_cols = inp_cols
        self.target_cols = target_cols
        self.date_col = date_col
        self.input_window = None
        self.output_window = None
        self.num_out_features = num_out_features
        self.stride = None
        self.y_mean = None
        self.y_std = None
        self.device = device
    
    def train_test_split(self, df, split_type='time', split_date='2021-12-31', split_ratio=None):
        '''

        split time series into train/test sets

        : param df:                     time array
        : para y:                       feature array
        : para split:                   percent of data to include in training set
        : return t_train, y_train:      time/feature training and test sets;
        :        t_test, y_test:        (shape: [# samples, 1])

        '''
        if split_type == 'time':
            df_train = df[df[self.date_col] <= split_date]
            df_test = df[df[self.date_col] > split_date]
            return df_train, df_test
        else:
            indx_split = int(split_ratio * df.shape[0])
            indx_train = np.arange(0, indx_split)
            indx_test = np.arange(indx_split, df.shape[0])
    
            df_train = df.iloc[indx_train]
            df_test = df.iloc[indx_test]

        return df_train.reset_index(drop='true'), df_test.reset_index(drop='true')
    
    def run_all_fn(self, 
                   df,
                   params,
                   stride=1):
        
        config = get_config()
        '''
        update config
        '''
        input_window=params['input_window']
        output_window=params['output_window']
        
        config['input_window']=params['input_window']
        config['output_window']=params['output_window']
        config['split_date']=params['split_date']
        config['epochs']=params['epochs']
        config['dropout']=params['dropout']
        config['num_layers']=params['num_layers']
        config['hidden_feature_size']=params['hidden_feature_size']
        config['weight_decay']=params['weight_decay']
        
        print("Performing train-test split ", end="...")
        df_train, df_test = self.train_test_split(df, split_date=config['split_date'])
        print("DONE\n")
        print("Normalizing the data ", end="...")
        df_train = self.normalize(df_train)
        df_test = self.normalize(df_test, use_stat=True)
        print("DONE\n")
        print("Creating windows ", end="...")
        X_train, Y_train = self.windowed_dataset(df_train, input_window, output_window, stride)
        X_test, Y_test = self.windowed_dataset(df_test, input_window, output_window, stride)
        print("DONE\n")
        
        print("Training the model ", end="...")
        model = seq2seq(input_size = X_train.shape[2],
                        hidden_size = config['hidden_feature_size'],
                        output_size=1,
                        model_type=config['model_type'],
                        num_layers = config['num_layers'],
                        utils=self,
                        dropout=config['dropout'],
                        device=self.device
                       )

        loss, test_rmse, train_rmse = model.train_model(X_train,
                                                        Y_train,
                                                        X_test,
                                                        Y_test,
                                                        target_len = config['output_window'],
                                                        config = config,
                                                        training_prediction = config['training_prediction'])
        print("DONE\n")
                
        train_eval_metrics = model.evaluate_batch(X_train.to(self.device), Y_train.to(self.device))
        test_eval_metrics = model.evaluate_batch(X_test.to(self.device), Y_test.to(self.device))

        print("Writing training data table.../n")
        train_T_pred_table, train_plot_df, train_plot_gt = self.predictionTable(df_train, train_eval_metrics)
        train_plot_df.to_csv('code/model_files/LSTM/LSTM_training_output.csv', index = False)
        
        print("Writing testing data table.../n")
        test_T_pred_table, test_plot_df, test_plot_gt = self.predictionTable(df_test, test_eval_metrics)
        test_plot_df.to_csv('code/model_files/LSTM/LSTM_testing_output.csv', index = False)

        
    def normalize(self, df, use_stat=False):
        '''
        Normalize data
        '''
        if use_stat:
            if len(set(self.inp_cols).intersection(self.target_cols))==0:
                df[self.inp_cols] = (df[self.inp_cols]-self.feat_mean)/self.feat_std
                df[self.target_cols] = (df[self.target_cols]-self.y_mean)/self.y_std
            else:
                df[self.inp_cols] = (df[self.inp_cols]-self.feat_mean)/self.feat_std
            return df
                
            # compute mean and std of target variable - to be used for unnormalizing
        self.y_std = df[self.target_cols].std()[0]
        self.y_mean = df[self.target_cols].mean()[0]
         
        if len(set(self.inp_cols).intersection(self.target_cols))==0:
            
            self.feat_mean = df[self.inp_cols].mean()
            self.feat_std = df[self.inp_cols].std()
            
            df[self.inp_cols] = (df[self.inp_cols]-self.feat_mean)/self.feat_std
            df[self.target_cols] = (df[self.target_cols]-self.y_mean)/self.y_std
        else:
            self.feat_mean = df[self.inp_cols].mean()
            self.feat_std = df[self.inp_cols].std()
            df[self.inp_cols] = (df[self.inp_cols]-self.feat_mean)/self.feat_std
            
        return df
        
    def windowed_dataset(self, df, input_window, output_window, stride):
        '''
        create a windowed dataset
    
        : param y:                time series feature (array)
        : param input_window:     number of y samples to give model
        : param output_window:    number of future y samples to predict
        : param stide:            spacing between windows
        : param num_features:     number of features (i.e., 1 for us, but we could have multiple features)
        : return X, Y:            arrays with correct dimensions for LSTM
        :                         (i.e., [input/output window size # examples, # features])
        '''
        self.input_window = input_window
        self.output_window = output_window
        self.stride = stride
        L = df.shape[0]
        num_samples = (L - self.input_window - self.output_window) // self.stride + 1
    
        dfX = df[self.inp_cols]
        dfY = df[self.target_cols]
        
        X = np.zeros([num_samples, self.input_window, self.num_features])
        Y = np.zeros([num_samples, self.output_window, self.num_out_features])
        # target_X = np.zeros([self.input_window, num_samples, self.num_out_features])
        
        # shuffled_inds = random.sample(range(num_samples),num_samples)
        
        for ii in np.arange(num_samples):
            start_x = self.stride * ii
            end_x = start_x + self.input_window
            X[ii, :, :] = dfX.iloc[start_x:end_x, :]
            
            start_y = self.stride * ii + self.input_window
            end_y = start_y + self.output_window
            Y[ii, :, :] = dfY.iloc[start_y:end_y, :]
        X = torch.from_numpy(X).type(torch.Tensor)
        Y = torch.from_numpy(Y).type(torch.Tensor)
        return X, Y
   

    def numpy_to_torch(self, Xtrain, Ytrain, Xtest, Ytest):
        '''
        convert numpy array to PyTorch tensor
    
        : param Xtrain:               windowed training input data (# examples, input window size, # features); np.array
        : param Ytrain:               windowed training target data (# examples, output window size, # features); np.array
        : param Xtest:                windowed test input data (# examples, input window size, # features); np.array
        : param Ytest:                windowed test target data (# examples, output window size, # features); np.array
        : return X_train_torch, Y_train_torch,
        :        X_test_torch, Y_test_torch:      all input np.arrays converted to PyTorch tensors

        '''

        X_train_torch = torch.from_numpy(Xtrain).type(torch.Tensor)
        Y_train_torch = torch.from_numpy(Ytrain).type(torch.Tensor)

        X_test_torch = torch.from_numpy(Xtest).type(torch.Tensor)
        Y_test_torch = torch.from_numpy(Ytest).type(torch.Tensor)

        return X_train_torch, Y_train_torch, X_test_torch, Y_test_torch
    
    
    def plot(self, df, pred_y, true_y, idx):
        '''
        param:
        
        Return:
        '''
        input_window = self.input_window
        output_window = self.output_window
        df_un = df.copy(deep=True)
        
        df_un[self.target_cols] = df_un[self.target_cols].apply(lambda l: l*self.y_std+self.y_mean)
        df_un = df_un.reset_index(drop=True)
        
        x_axis_1 = df_un.loc[idx:idx+input_window-1,self.date_col].to_numpy().reshape(-1)
        
        x_axis_2 = df_un.loc[idx+input_window:idx+input_window+output_window-1,self.date_col].to_numpy().reshape(-1)
        
        x_plot = df_un[self.target_cols].to_numpy()[idx:idx+input_window].reshape(-1)
        
        pred_plot = pred_y[idx].cpu().numpy().reshape(-1)
        
        true_plot = true_y[idx].cpu().numpy().reshape(-1)
    
        plt.figure(figsize=(20,4), dpi=150)
        plt.grid("on", alpha=0.4)
        plt.plot(x_axis_1, x_plot, marker='o', linestyle='--', label='input_window')
        plt.plot(x_axis_2, pred_plot, marker='o', linestyle='--', label='predictions')
        plt.plot(x_axis_2, true_plot, marker='o', linestyle='--', label='true_values')
        plt.xlabel('Timeline')
        plt.ylabel('Chlorophyll')
        plt.xticks(rotation=90)
        plt.legend()
    
    
    def plot_samples(self, df, pred_y, true_y, idx, n_samples, xticks_spacing=False):
        '''
        params:
        
        return:
        '''
    
        input_window = self.input_window
        output_window = self.output_window
        df_un = df.copy(deep=True)
        
        df_un[self.target_cols] = df_un[self.target_cols].apply(lambda l: l*self.y_std+self.y_mean)
        df_un = df_un.reset_index(drop=True)
        
        x_axis_1 = df_un.loc[idx:idx+input_window-1,self.date_col].to_numpy().reshape(-1)
        
        start_x_axis = idx + input_window
        delta = n_samples*output_window - 1 if n_samples*output_window - 1 < (df_un.shape[0]-start_x_axis) else df_un.shape[0]-start_x_axis
        
        end_x_axis = start_x_axis + delta #n_samples*output_window - 1
        
        x_axis_2 = df_un.loc[start_x_axis:end_x_axis,self.date_col].to_numpy().reshape(-1)
            
        x_plot = df_un[self.target_cols].to_numpy()[idx:idx+input_window].reshape(-1)
        
        pred_plot = pred_y[idx:idx+n_samples*output_window:output_window].cpu().numpy().reshape(-1)
        true_plot = true_y[idx:idx+n_samples*output_window:output_window].cpu().numpy().reshape(-1)
    
        fig,ax = plt.subplots()
        
        fig.set_figheight(5)
        fig.set_figwidth(20)

        ax.grid(visible=True, alpha=0.2)
        ax.plot(x_axis_1, x_plot, linestyle='--', label='input_window')
        ax.plot(x_axis_2, pred_plot, linestyle='--', label='predictions')
        ax.plot(x_axis_2, true_plot, linestyle='--', label='true_values')
        ax.set_xlabel('Timeline')
        ax.set_ylabel('Chlorophyll')
        
        if xticks_spacing:
            every_nth = 20
            for n, label in enumerate(ax.xaxis.get_ticklabels()):
                if n % every_nth != 0:
                    label.set_visible(False)

        ax.set_xticklabels(x_axis_1, rotation=90)
        ax.set_xticklabels(x_axis_2, rotation=90)
        plt.legend()
        
    
    def pred_per_step_helper(self, predictions, idx, pred_values, date):
        '''
        Compute all the predictions for a single date. i.e. as T+1, T+2, T+3, ... T+horizon timestep prediction
        '''
        c = 0
        rind = idx
        while c<self.output_window:
            rind = rind + c
            pred_values[date].append(predictions[rind].reshape(-1)[-(c+1)])
            c+=1
    
        return pred_values

    def prediction_per_step(self, df, predictions, gts, ids):
        pred_values = {}
        gts_ls = {}
        for idx in ids:
            date = df.loc[idx+self.input_window+self.output_window-1,self.date_col]
            pred_values[date] = []
            gts_ls['GT_'+date] = gts[idx].reshape(-1)[-1]
            pred_values = self.pred_per_step_helper(predictions, idx, pred_values, date)
    
        pred_values = {k:list(reversed(v)) for k,v in pred_values.items()}
        for k,v in pred_values.items():
            pred_values[k] = [i.cpu().numpy() for i in v]
            
        return pred_values, gts_ls
    
    
    def plot_time_step_predictions(self, pred_values, gt):
        
        """
        Currently the number of plots is hard-configured to 6
        """
        x_axis_label = 'T+'
        x_axis = [x_axis_label+str(i) for i in range(1,self.output_window+1)]
        
        fig, axes = plt.subplots(2, 3, figsize=(25, 15), dpi=150)
        for i,v in enumerate(pred_values.items()):
            ax = axes[i//3, i%3]
            ax.grid(alpha=0.7)
            ax.plot(x_axis, v[1], marker='o', linestyle='dashed', linewidth=2, markersize=12)
            gt_ind_label = 'GT_'+v[0]
            ax.axhline(gt[gt_ind_label].cpu().numpy())
            ax.set_xlabel(f"For date = {v[0]}, ground-truth was = {gt[gt_ind_label]}")
            ax.set_ylabel('Predicted values')
    
    def fillpredtable(self, r, table, pred):
        for i,k in enumerate(table.columns):
            if i-(r-1)>=0 and i-(r-1) < pred.shape[0]:
                table.loc[r, k] = pred[i-(r-1)][r-1].cpu().numpy()
        return table
            
    def predictionTable(self, df, eval_dict, plot=True):
        '''
        Create the prediction table
        '''
        gt = eval_dict['y_true']
        gt_df = pd.DataFrame(gt.cpu().numpy()[:,:,0])
        gt_values = np.append(gt_df[0].values, gt_df.iloc[-1,1:]) # ground-truth values for train data

        pred_df = eval_dict['y_pred'] # model predicted values for train data

        pred_table = np.zeros((self.output_window, df.shape[0] - self.input_window))
        pred_table = pd.DataFrame(pred_table)
        pred_table.columns = df[self.input_window:].datetime.values
        pred_table.index = range(1,self.output_window+1)
        pred_table.loc[:] = np.nan
        
        for r in range(1, self.output_window+1):
            pred_table = self.fillpredtable(r, pred_table, pred_df)

        if plot:    
            plot_df = pred_table.iloc[:, self.output_window-1:-self.output_window+1]
            plot_gt_values = gt_values[self.output_window-1:-self.output_window+1]
            return pred_table, plot_df, plot_gt_values

        return pred_table

    def plot_predictions(self, df, eval_dict, T, split='Train'):
        '''
        Plot the prediction table
        '''
        train_T_pred_table, plot_df, plot_gt = self.predictionTable(df, eval_dict)

        x_plot = plot_df.columns
        fig,ax = plt.subplots()
        
        fig.set_figheight(5)
        fig.set_figwidth(20)
        
        ax.grid(visible=True, alpha=0.2)
        
        for t in T:
            y_axis = plot_df.loc[t,:].values
            ax.plot(x_plot, y_axis, linestyle='--', label='T+'+str(t))
        
        ax.plot(x_plot, plot_gt, linestyle='--', label='Ground-truth')
        ax.set_xlabel('Timeline')
        ax.set_ylabel('Chlorophyll T+n predictions')
        plt.title(f'Clorophyll-a Prediction Performance on {split} data')
        every_nth = 20
        for n, label in enumerate(ax.xaxis.get_ticklabels()):
            if n % every_nth != 0:
                label.set_visible(False)

        ax.set_xticklabels(x_plot, rotation=90)
        plt.legend()
    
    def compute_rmse(self, i, ptable, gt_values):
    
        tk = ptable.iloc[i,:].values
        null_inds = np.where(np.isnan(tk))[0]
        mask = np.ones(gt_values.shape)
        mask[null_inds]= 0
        tk = np.nan_to_num(tk)
        
        unreduced_loss = (tk-gt_values)**2
        unreduced_loss = (unreduced_loss * mask).sum()
        
        non_zero_elements = mask.sum()
        loss = unreduced_loss / non_zero_elements
        
        rmse = loss**0.5
        return rmse

    def plot_RMSE_epochs(self, test_rmse, train_rmse):
        plt.figure(figsize=(15, 5))  # Adjusted figure size and dpi for better quality
        plt.plot(train_rmse, lw=2.0, label='Train RMSE', color='blue')  # Added color for better readability
        plt.plot(test_rmse, lw=2.0, label='Test RMSE', color='orange')  # Added color for better readability
  
        if len(train_rmse) > 200:
            stride=10
        elif len(train_rmse) >= 500:
            stride=20
        else:
            stride=1
        # Set x-axis ticks with one stride
        plt.xticks(range(0, len(train_rmse), stride), rotation=90)
  
        # plt.yscale("log")
        plt.grid(True, which="both", linestyle='--', linewidth=0.5, alpha=0.5)  # Changed grid format for clarity
        plt.xlabel('Epochs')
        plt.ylabel('Root Mean Squared Error (RMSE)')
        plt.title('Train and Test RMSE over Epochs')  # Adjusted title for clarity
        plt.legend()
        plt.tight_layout()  # Adjust layout for better spacing
        plt.show()

    def calculate_RMSE_horizon(self, df, eval_dict):
        T_pred_table, plot_df, plot_gt = self.predictionTable(df, eval_dict)
        gt = eval_dict['y_true']
        gt_df = pd.DataFrame(gt.cpu().numpy()[:,:,0])
        gt_values = np.append(gt_df[0].values, gt_df.iloc[-1,1:]) # ground-truth values for train data
        gt_values = np.append(gt_df[0].values, gt_df.iloc[-1,1:]) # ground-truth values for train data
        rmse_values = []
        for i in range(self.output_window):
            rmse_values.append(self.compute_rmse(i, T_pred_table, gt_values))
        rmse_values = pd.DataFrame(rmse_values, columns=['RMSE'], index=range(1,self.output_window+1))
        return rmse_values

    def plot_RMSE_horizon(self, train_rmse_values, test_rmse_values, T):
        fig, ax = plt.subplots(figsize=(5, 5))
        ax.plot(range(1, T+1), train_rmse_values, label='train RMSE')
        ax.plot(range(1, T+1), test_rmse_values, label='test RMSE')

        plt.xticks([i for i in range(1, T, 1)])
        ax.grid(visible=True, alpha=0.5)

        ax.set_xlabel('T+n Horizon Window')
        ax.set_ylabel('RMSE')
        ax.set_title('RMSE for train and test')
        ax.legend()
        plt.tight_layout()
        plt.show()
