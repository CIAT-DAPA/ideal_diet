


path <- "G:/CIAT/Code/CWR/ideal_diet/src/inputs/data_m664.csv"

data <-  read.csv(path,dec=".",header=T,sep=",",stringsAsFactors = F)


data_l <- split(data,data$Group)

data=data_l[[2]]

quan <- function (data){
  
  if (data[1,5]=="A"){
    pos=which(data[,3]>=data[1,4])
    quan=quantile(data[-pos,3],p=c(0.25,0.50,0.75))
    
    c_group=ifelse(data[,3]>=data[1,4],"5",ifelse( data[,3]<data[1,4]&data[,3]>quan[3],"4", ifelse( data[,3]<quan[3]&data[,3]>quan[2] ,"3", ifelse(data[,3]<quan[2]&data[,3]>quan[1],"2","1"))))
    
  }else{
    pos=which(data[,3]<=data[1,4])
    quan=quantile(data[-pos,3],p=c(0.75,0.50,0.25))
    
    c_group=ifelse(data[,3]<=data[1,4],"5",ifelse( data[,3]>data[1,4]&data[,3]<quan[3],"4", ifelse( data[,3]>quan[3]&data[,3]<quan[2] ,"3", ifelse(data[,3]>quan[2]&data[,3]<quan[1],"2","1"))))
    
  }

  data_G <- data.frame(G1=quan[1],G2=quan[2],G3=quan[3],G4=data[1,4],row.names ="")
  
  data_final=cbind.data.frame(data,data_G,category=c_group)
  
  return(data_final)  
  
}

all_g <- lapply(data_l,quan) %>% do.call("rbind",.)

write.csv(x = all_g,file = "G:/CIAT/Code/CWR/ideal_diet/src/outputs/category.csv",row.names = F, col.names = T)

