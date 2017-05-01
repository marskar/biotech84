val <- 3
vec <- c(1, 2, 3, 4, 5, 6)
ls <- list(1, 2, 3, "a", "b", "c")
m<- matrix(c(1,2,3,4,5,6), nrow =2)
df <- data.frame(x = 1:3, y = c("a", "b", "c"))

###You can add names:
  
names(ls)<-c("A","B","C","D","E","F")
names(ls)<-LETTERS[1:6]
colnames(m)<- LETTERS[1:2]
rownames(m)<- LETTERS[24:26]

###You can rename columns and rows
colnames(df)<- c("A","B")
colnames(df)<- LETTERS[1:2]

### Add an unnamed third column to df and m
df[,3]<-1:3
m[,3]<-1:3

### Add df to m as additional columns:
  dfmc<-cbind(df,m)
### cbind will only work if row names match!
  
##Add a column named D to df
df$D<-1:4
### This will not work for m!
  
### Add an unnamed 4th row to df and m:
  df[4,]<-1:4
m[4,]<-1:4

### Add m to df as additional rows:
  dfmr<-rbind(df,m)
### rbind will only work if column names match!
  
## To delete the first column of m:
    m <- m[ ,-1]
  
## To delete the B column of m:
    m <- m[-B]
  
### To delete the first row of m:
    m <- m[-1, ]
  
### To remove everything except column B in df:
    df<-df$B
  
  
