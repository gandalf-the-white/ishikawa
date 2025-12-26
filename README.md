
# Table of Contents

1.  [Pseudo-code](#org996b589)
    1.  [Matrix transpose](#orgafbee16)
    2.  [Matrix Multiplication](#org07a2c92)
2.  [Blobs](#org8dea47d)
    1.  [Objective](#org8b0a8ff)
3.  [Perceptron](#org7d66751)
    1.  [Linear function](#org8d4eb44)
    2.  [Sigmoid function](#org5259659)
    3.  [Bernoulli](#orga2ce57a)
4.  [Graphic](#orgb2291e6)
    1.  [Overview](#org8bc73c6)
    2.  [Neuron Network](#org332b37f)
    3.  [Log Loss Convergence](#org5913e7a)
5.  [Cost function](#orgfbeef6b)
    1.  [Likelihood (*Vraisemblance*)](#orgc8763c1)
    2.  [Loss function (Log Loss)](#org1c7e249)
    3.  [Log Likelihood](#org9f0a892)
    4.  [Result](#org93f17e0)
6.  [Gradient descent](#org6e3cc47)
    1.  [Gradient descent algorithm](#orgd70858e)
    2.  [Decomposition](#orgb690674)
    3.  [Calculation of $\frac{\delta \mathcal{L}}{\delta a}$](#org38bbe65)
    4.  [Calculation of $\frac{\delta a}{\delta z}$](#org40771d2)
    5.  [Calculation of $\frac{\delta z}{\delta w_1}$](#org36b4290)
    6.  [Calculation of $\frac{\delta z}{\delta w_2}$](#org60c88d9)
    7.  [Calculation of $\frac{\delta z}{\delta b}$](#org6bd21fd)
    8.  [Conclusion for $\frac{\delta \mathcal{L}}{\delta w_1}$](#orgbf45ff4)
    9.  [Conclusion for $\frac{\delta \mathcal{L}}{\delta w_2}$](#orge02d438)
    10. [Conclusion for $\frac{\delta \mathcal{L}}{\delta b}$](#orgedc7883)
    11. [Conclusion](#orgc117fe1)
7.  [Vector](#orgbbee2e2)
    1.  [Introduction](#orga57c6db)
    2.  [Dataset](#org3703141)
    3.  [Vectorization of A](#org85aeb2f)
    4.  [Cost function Vectorization](#org33ece96)
    5.  [Gradient descent Vectorization](#org33d7170)
    6.  [Gradient Vectorization](#orgddab071)
8.  [Algorithm](#orga3d4f33)
    1.  [Overview](#org10acb9c)
    2.  [Algorithme](#org4dc1660)



<a id="org996b589"></a>

# Pseudo-code


<a id="orgafbee16"></a>

## Matrix transpose

    Function transpose_matrix(M):
        m = number of rows in M
        n = number of columns in M
        T = empty matrix
    
        For j from 0 to n-1:
            Initialize row as an empty list
            For i from 0 to m-1:
                Append M[i][j] to row
            Append row to T
    
        Return T


<a id="org07a2c92"></a>

## Matrix Multiplication

    Function multiply-matrices(matrix1, matrix2):
        m = number of rows in matrix1
        n = number of columns in matrix1
        p = number of columns in matrix2
    
        If n ≠ number of rows in matrix2:
            Return "Error: incompatible dimensions"
    
        Initialize result as an empty list
    
        For i from 0 to m-1:
            Initialize row as an empty list
            For j from 0 to p-1:
                sum = 0
                For k from 0 to n-1:
                    sum = sum + matrix1[i][k] * matrix2[k][j]
                Append sum to row
            Append row to result
    
        Return result


<a id="org8dea47d"></a>

# Blobs


<a id="org8b0a8ff"></a>

## Objective

Build a data set with:

-   X: 2D points (or N dimensions)
-   y: cluster label

Each cluster is centralize around one center with gaussian noise.
Just a simple example

![img](images/blobs.png)


<a id="org7d66751"></a>

# Perceptron


<a id="org8d4eb44"></a>

## Linear function


<a id="org5259659"></a>

## Sigmoid function


<a id="orga2ce57a"></a>

## Bernoulli


<a id="orgb2291e6"></a>

# Graphic


<a id="org8bc73c6"></a>

## Overview

![img](images/perceptron.png)


<a id="org332b37f"></a>

## Neuron Network

![img](images/network.png)


<a id="org5913e7a"></a>

## Log Loss Convergence

![img](images/loss.png)


<a id="orgfbeef6b"></a>

# Cost function


<a id="orgc8763c1"></a>

## Likelihood (*Vraisemblance*)


<a id="org1c7e249"></a>

## Loss function (Log Loss)


<a id="org9f0a892"></a>

## Log Likelihood


<a id="org93f17e0"></a>

## Result


<a id="org6e3cc47"></a>

# Gradient descent

This involves adjusting the parameters \textbf{W} and \textbf{b} in order to minimize model errors, i.e., to minimize the \textbf{cost function} (Log Loss).  
That is why we calculate the gradient (or derivative) of the \textbf{cost function}.


<a id="orgd70858e"></a>

## Gradient descent algorithm


<a id="orgb690674"></a>

## Decomposition


<a id="org38bbe65"></a>

## Calculation of $\frac{\delta \mathcal{L}}{\delta a}$


<a id="org40771d2"></a>

## Calculation of $\frac{\delta a}{\delta z}$

Let us choose $h=g \circ f$ with $f(z) = 1+e^{-z}$ and $g(f(z))=\frac{1}{f(z)}$, i.e.

In conclusion, we obtain


<a id="org36b4290"></a>

## Calculation of $\frac{\delta z}{\delta w_1}$


<a id="org60c88d9"></a>

## Calculation of $\frac{\delta z}{\delta w_2}$


<a id="org6bd21fd"></a>

## Calculation of $\frac{\delta z}{\delta b}$


<a id="orgbf45ff4"></a>

## Conclusion for $\frac{\delta \mathcal{L}}{\delta w_1}$


<a id="orge02d438"></a>

## Conclusion for $\frac{\delta \mathcal{L}}{\delta w_2}$


<a id="orgedc7883"></a>

## Conclusion for $\frac{\delta \mathcal{L}}{\delta b}$


<a id="orgc117fe1"></a>

## Conclusion

For our gradient descent

we obtain the following gradients:

That mean:


<a id="orgbbee2e2"></a>

# Vector


<a id="orga57c6db"></a>

## Introduction

In mathematics and programming, a column vector is usually represented as a vertical list of elements, enclosed in square brackets or parentheses. In Lisp (and more specifically in Common Lisp), there is no native data type for column vectors as in mathematics or NumPy. However, you can represent a column vector as a simple list, where each element corresponds to a component of the vector.


<a id="org3703141"></a>

## Dataset

Let&rsquo;s take the case of a dataset containing m data points, each of which consists of n parameters (variables).

and

as well as

For $n=2$, we therefore find that

or


<a id="org85aeb2f"></a>

## Vectorization of A

Reminder

So, we have


<a id="org33ece96"></a>

## Cost function Vectorization

The objective is to compare vector $A$ with vector $y$.


<a id="org33d7170"></a>

## Gradient descent Vectorization

Remind

So, we can write

Mathematically, we should rather write  $W_{t+1} = W_t - \alpha \frac{\delta \mathcal{L}} {\delta W}$


<a id="orgddab071"></a>

## Gradient Vectorization

and


<a id="orga3d4f33"></a>

# Algorithm


<a id="org10acb9c"></a>

## Overview

![img](images/algorithm.png)


<a id="org4dc1660"></a>

## Algorithme

![img](images/algo.png)

