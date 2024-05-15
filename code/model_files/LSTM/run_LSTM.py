import random
import pandas as pd
import numpy as np
from tqdm import trange
import datetime
import matplotlib.pyplot as plt

import torch
import torch.nn as nn
from torch import optim
from encoder_decoder import seq2seq

import warnings
warnings.filterwarnings('ignore')

if torch.cuda.is_available():
    device = torch.device('cuda')
    print("Computation device: GPU")
else:
    device = torch.device('cpu')
    print("WARNING: For this notebook to perform best, "
        "if possible, in the menu under Runtime -> "
        "Change runtime type. select GPU. ")

seed = 2024

np.random.seed(seed)

torch.manual_seed(seed)
torch.cuda.manual_seed_all(seed)
torch.cuda.manual_seed(seed)
torch.backends.cudnn.benchmark = False
torch.backends.cudnn.deterministic = True

params = {
    'epochs': 100,
    'input_window': 14,
    'output_window': 7,
    'weight_decay': 0.05,
    'split_ratio': ...
}

# Run and generate predictions
run_all(params)
