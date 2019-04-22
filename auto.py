#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun  7 13:14:30 2018

@author: ajay
"""
#importing library
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Embedding
from keras.layers import LSTM
from keras.layers import Dropout
import math
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error


os.chdir("/home/ajay/AJAY/My working Files/Udemy Jupyter/rnn_model")

#importing the dataset
train_data = pd.read_csv('auto_train.csv')
train_set = train_data.iloc[:,1:2].values

# Feature Scaling Normalization. It is best to use normalization when sigmoid is
# used as an activation function.
sc = MinMaxScaler(feature_range = (0,1))
train_set_scaled = sc.fit_transform(train_set)

# Creating a data structure with timestap 60 days and predicting the stock price for 
# 61th day

x_train = []
y_train = []
for i in range(60,1738):
    x_train.append(train_set_scaled[i-60:i,0])
    y_train.append(train_set_scaled[i,0])
x_train , y_train = np.array(x_train), np.array(y_train)

#Reshaping
x_train = np.reshape(x_train , (x_train.shape[0],x_train.shape[1],1))

#Building the model
#Initialising the RNN

regressor = Sequential()

#Adding the LSTM layers and some Dropout regularisation to avoid overfitting
# First Layer
regressor.add(LSTM(units = 60, return_sequences = True,input_shape = (x_train.shape[1],1)))
regressor.add(Dropout(rate = 0.2))

# Second Layer
regressor.add(LSTM(units = 60, return_sequences = True))
regressor.add(Dropout(rate = 0.2))


# Third Layer
regressor.add(LSTM(units = 60, return_sequences = True))
regressor.add(Dropout(rate = 0.2))


# Fourth Layer
regressor.add(LSTM(units = 60, return_sequences = False))
regressor.add(Dropout(rate = 0.2))

# Output Layer
regressor.add(Dense(units = 1))

#Compiling RNN
regressor.compile(optimizer='adam', loss = 'mean_squared_error')

#fitting the rnn model to the training set
regressor.fit(x_train, y_train, epochs = 70, batch_size = 32)

#making predictions and visualizing the results
#Getting the real stock prices of google 2017
test_data = pd.read_csv("auto_test.csv")
actual_set = test_data.iloc[:,1:2].values

len(actual_set)

#Getting the predicted stock prices of google
total_data = pd.concat((train_data['Auto'],test_data['Auto']),axis = 0)
inputs = total_data[len(total_data)-len(test_data)-60:].values
inputs = inputs.reshape(-1,1)
inputs = sc.transform(inputs)
len(test_data)
len(total_data)
x_test = []
for i in range(60,308):
    x_test.append(inputs[i-60:i,0])
x_test= np.array(x_test)
x_test = np.reshape(x_test,(x_test.shape[0],x_test.shape[1],1))
predicted_set = regressor.predict(x_test)
predicted_set = sc.inverse_transform(predicted_set)
len(predicted_set)

#Visualizing the results
plt.plot(actual_set , color = 'red', label = 'Real Auto Price')
plt.plot(predicted_set , color = 'blue' , label = 'Predicted Auto Price')
plt.xlabel('Time')
plt.ylabel('Auto Stock Price')
plt.legend()
plt.show()

mean_absolute_error(actual_set,predicted_set)
mean_squared_error(actual_set,predicted_set)















