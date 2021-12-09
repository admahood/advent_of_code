library(tidyverse)

read_lines("2021/day9input.txt")[1] %>% nchar

input<-read_fwf("2021/day9input.txt", fwf_widths(rep(1,100))) %>% as.matrix

# part 1

result<- c()
counter=1
for(r in 1:nrow(input)) {
  for(c in 1:ncol(input)){
    neighbors<-c()
    cell <- input[r,c] %>% as.numeric()
    
    if(r>1) neighbors[1] <- input[r-1, c] %>% as.numeric()
    if(r<100) neighbors[2] <- input[r+1, c] %>% as.numeric()
    if(c>1) neighbors[3] <- input[r, c-1] %>% as.numeric()
    if(c<100) neighbors[4] <- input[r, c+1] %>% as.numeric()
    
    if(cell < min(neighbors, na.rm = T)){
      result[counter]<- cell+1
      counter <- counter+1
    }
    
  }
}
sum(result)

# part 2
output <- input
for(r in 1:nrow(input)) {
  for(c in 1:ncol(input)){
    if(output[r,c]==9) output[r,c]<-NA else output[r,c]<- 0
    
  }
}

counter = 1
for(r in 1:nrow(output)) {
  for(c in 1:ncol(output)){
    neighbors<-c()
    cell <- output[r,c] %>% as.numeric()
    
    if(!is.na(cell)){
      if(r>1) neighbors[1] <- output[r-1, c] %>% as.numeric()
      if(r<100) neighbors[2] <- output[r+1, c] %>% as.numeric()
      if(c>1) neighbors[3] <- output[r, c-1] %>% as.numeric()
      if(c<100) neighbors[4] <- output[r, c+1] %>% as.numeric()
      neighbors[5] <- cell
      
      

      if(cell == 0 & sum(neighbors, na.rm=T) == 0){
        output[r,c] <- counter
        if(r>1) if(!is.na(output[r-1, c])) output[r-1, c] <- counter
        if(r<100) if(!is.na(output[r+1, c])) output[r+1, c] <- counter
        if(c>1) if(!is.na(output[r, c-1])) output[r, c-1] <- counter
        if(c<100) if(!is.na(output[r, c+1])) output[r, c+1] <- counter
        counter = counter+1
        next
      }
      
      if(cell == 0 & length(unique(na.omit(neighbors[neighbors>0])))==1){
        output[r,c] <- max(neighbors,na.rm=T)
        if(r>1) if(!is.na(output[r-1, c])) output[r-1, c] <- max(neighbors, na.rm=T)
        if(r<100) if(!is.na(output[r+1, c])) output[r+1, c] <- max(neighbors, na.rm=T)
        if(c>1) if(!is.na(output[r, c-1])) output[r, c-1] <- max(neighbors, na.rm=T)
        if(c<100) if(!is.na(output[r, c+1])) output[r, c+1] <- max(neighbors, na.rm=T)
        next
        }
        
      if(cell == 0 & length(unique(na.omit(neighbors[neighbors>0])))>1){         
        output[r,c] <- max(neighbors,na.rm=T)
        if(r>1) if(!is.na(output[r-1, c])) output[r-1, c] <- max(neighbors, na.rm=T)
        if(r<100) if(!is.na(output[r+1, c])) output[r+1, c] <- max(neighbors, na.rm=T)
        if(c>1) if(!is.na(output[r, c-1])) output[r, c-1] <- max(neighbors, na.rm=T)
        if(c<100) if(!is.na(output[r, c+1])) output[r, c+1] <- max(neighbors, na.rm=T)
        next
        }
        
      if(cell != 0 & length(unique(na.omit(neighbors[neighbors>0])))>1){
        if(r>1) if(!is.na(output[r-1, c])) output[r-1, c] <- max(neighbors, na.rm=T)
        if(r<100) if(!is.na(output[r+1, c])) output[r+1, c] <- max(neighbors, na.rm=T)
        if(c>1) if(!is.na(output[r, c-1])) output[r, c-1] <- max(neighbors, na.rm=T)
        if(c<100) if(!is.na(output[r, c+1])) output[r, c+1] <- max(neighbors, na.rm=T)
        
        for(n in na.omit(unique(neighbors[neighbors>0]))) output[output == n] <- max(neighbors[neighbors>0], na.rm=T)
        
        next
      }
      if(cell != 0 & length(unique(na.omit(neighbors[neighbors>0])))==1){
        if(r>1) if(!is.na(output[r-1, c])) output[r-1, c] <- max(neighbors, na.rm=T)
        if(r<100) if(!is.na(output[r+1, c])) output[r+1, c] <- max(neighbors, na.rm=T)
        if(c>1) if(!is.na(output[r, c-1])) output[r, c-1] <- max(neighbors, na.rm=T)
        if(c<100) if(!is.na(output[r, c+1])) output[r, c+1] <- max(neighbors, na.rm=T)
        
        # for(n in na.omit(unique(neighbors[neighbors>0]))) output[output == n] <- max(neighbors[neighbors>0], na.rm=T)
        
        next
      }
      
        
      
    }
  }
}

table(output) %>% as.data.frame()%>% arrange(desc(Freq))%>%slice(1:3) %>% pull(Freq)
106*105*102
