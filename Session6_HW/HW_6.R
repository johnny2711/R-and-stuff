#Matrix Manipulation

#Q1

require(MASS)
A=matrix(data=c(1,3,2,4), nrow=2)
print(A)
B=matrix(c(5,7,6,8), nrow=2)
print(B)
det(A)
det(B)
#As the determinants are non-zero, an inverse of both matrices exists

#Q2

#To compute the inverse of each matrix, we first need the cofactor matrices 
minors_A=matrix(data=c(4,2,3,1),nrow=2)
minors_B=matrix(data=c(8,6,7,5), nrow=2)

#The adjugate matrix is simply the transpose of the cofactor matrix
adjugate_A=t(minors_A)
adjugate_B=t(minors_B)

#Now we can get the inverse by dividing the adjugate matrix through the determinant
inverse_A=adjugate_A/det(A)
print(inverse_A)
inverse_B=adjugate_B/det(B)
print(inverse_B)

#To verify our answer, we can use the built-in function from the package "MASS"
ginv(A)
ginv(B)

#Q5
a1=c(1,3)
a2=c(2,4)
b1=c(5,7)
b2=c(6,8)
cbind(a1,a2)
cbind(b1,b2)

#Q6
solve(A)
solve(B)

#Q7
invAxB=inverse_A*B
invBxB=inverse_B*B
