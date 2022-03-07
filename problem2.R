# problem 2

# 1. KNN
library(ggplot2)
library(class)

labeled_data<-readRDS("C:\\Users\\ALIENWARE\\Desktop\\hw2\\Homework2.labeled.data")
x = labeled_data[1]
x = matrix(unlist(x), ncol =3, nrow =200)

y = labeled_data[2]
y = matrix(unlist(y), ncol =1, nrow =200)

x0 = x[1:99,]
x1 = x[100:200,]
y0 = y[1:99]
y1 = y[100:200]
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

Table <- knn(x0,x1,cl=y0,k=9)
Table <- table(Table,y1)
Table

accuracy(Table)


write.table(x, file="C:\\Users\\ALIENWARE\\Desktop\\hw2\\x.txt", row.names=FALSE, col.names=FALSE)
write.table(y, file="C:\\Users\\ALIENWARE\\Desktop\\hw2\\y.txt", row.names=FALSE, col.names=FALSE)

# matlab code below for Baye's Classifier
clear all
clc

x = load("C:\Users\ALIENWARE\Desktop\hw2\x.txt");
y = load("C:\Users\ALIENWARE\Desktop\hw2\y.txt");
x1=zeros(3);x2=zeros(3);x3=zeros(3);x4=zeros(3);
c1=1;c2=1;c3=1;c4=1;
for i = 1:200
    switch y(i)
    case 1
        x1(c1,:) = x(i,:);
        c1=c1+1;
    case 2
        x2(c2,:) = x(i,:);
        c2=c2+1;
    case 3
        x3(c3,:) = x(i,:);
        c3=c3+1;
    case 4
        x4(c4,:) = x(i,:);
        c4=c4+1;
    end
end
data_size = 200;
m1 = mean(x1);
m2 = mean(x2);
m3 = mean(x3);
m4 = mean(x4);

cov1 = x1'*x1/data_size;
cov2 = x2'*x2/data_size;
cov3 = x3'*x3/data_size;
cov4 = x4'*x4/data_size;

cov1 = diag(diag(cov1));
cov2 = diag(diag(cov2));
cov3 = diag(diag(cov3));
cov4 = diag(diag(cov4));

p1=0;p2=0;p3=0;p4=0;

for i = 1:200
    x0 = x(i,:);
    p1(i)=det(cov1)^(-1.5)*exp(-0.5*(x0-m1)*cov1^(-1)*(x0-m1)');
end

for i = 1:200
    x0 = x(i,:);
    p2(i)=det(cov2)^(-1.5)*exp(-0.5*(x0-m2)*cov2^(-1)*(x0-m2)');
end

for i = 1:200
    x0 = x(i,:);
    p3(i)=det(cov3)^(-1.5)*exp(-0.5*(x0-m3)*cov3^(-1)*(x0-m3)');
end

for i = 1:200
    x0 = x(i,:);
    p4(i)=det(cov4)^(-1.5)*exp(-0.5*(x0-m4)*cov4^(-1)*(x0-m4)');
end

for i = 1:200
    ps = [p1(i),p2(i),p3(i),p4(i)];
    index = find(ps==max(ps));
    y_pred(i) = index;
end

cm = zeros(4,4);

for i = 1:200
    switch y(i)
    
        case 1
                switch y_pred(i)
    
                case 1
                        cm(1,1) = cm(1,1) + 1;
                case 2
                        cm(1,2) = cm(1,2) + 1;
                case 3
                        cm(1,3) = cm(1,3) + 1;
                case 4
                        cm(1,4) = cm(1,4) + 1;
                end
            
        case 2
                switch y_pred(i)
    
                    case 1
                            cm(2,1) = cm(2,1) + 1;
                    case 2
                            cm(2,2) = cm(2,2) + 1;
                    case 3
                            cm(2,3) = cm(2,3) + 1;
                    case 4
                            cm(2,4) = cm(2,4) + 1;
                end

        case 3
                switch y_pred(i)

                    case 1
                    cm(3,1) = cm(3,1) + 1;
                    case 2
                    cm(3,2) = cm(3,2) + 1;
                    case 3
                    cm(3,3) = cm(3,3) + 1;
                    case 4
                    cm(3,4) = cm(3,4) + 1;
                end

        case 4
            switch y_pred(i)

                case 1
                cm(4,1) = cm(4,1) + 1;
                case 2
                cm(4,2) = cm(4,2) + 1;
                case 3
                cm(4,3) = cm(4,3) + 1;
                case 4
                cm(4,4) = cm(4,4) + 1;
            end
    end
end

sum(diag(cm))/sum(sum(cm))
% 0.6750
% 0.6800