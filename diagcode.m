clear all
clc

%Creating the matrix
a=8/10
b=1/10
c=1/10
d=0
e=1/10
f=5/10
g=2/10
h=2/10
i=1/10
j=3/10
k=3/10
l=3/10
m=0
n=0
p=0
q=1

L=[a,b,c,d;
    e,f,g,h;
    i,j,k,l;
    m,n,p,q]

%Diagonalizing the matrix
[M,D]=eig(L)

%Question 1a:
twyr=M*(D.^2)*inv(M)

%Question 1b:
month=M*(D.^(1/12))*inv(M)
