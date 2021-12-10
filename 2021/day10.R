# day 10
library(tidyverse)
input<- read_lines("2021/day10input.txt")


# part 1
opens <- c("\\{",
         "\\(",
         "\\[",
         "\\<")

pairs <- c("\\<\\>", "\\[\\]", "\\(\\)", "\\{\\}")

result <- c()
for(i in 1:length(input)){
  row <- input[i]
  
  while(any(str_detect(row, pairs))){
    for(p in pairs) row<-str_remove_all(row, p)
    }
  print(row)
  for(o in opens) row<-str_remove_all(row, o)
  if(nchar(row)>0) result[i] <- str_sub(row,1,1)
}

output <- result %>%
  na.omit() %>%
  as_tibble() %>%
  mutate(score = case_when(
    value == "]" ~ 57,
    value == ")" ~ 3,
    value == "}" ~ 1197,
    value == ">" ~ 25137
  )) %>%
  pull(score) %>%
  sum()

# part 2

input2 <- input[which(is.na(result))]

lut_closed <- c("{"="}",
           "(" = ")",
           "[" = "]",
           "<" = ">")


lut_score<- c( ")"= 1,
"]" =2,
"}" =3,
">" =4)


pairs <- c("\\<\\>", "\\[\\]", "\\(\\)", "\\{\\}")

result2 <- c()
for(i in 1:length(input2)){
  row <- input2[i]
  
  while(any(str_detect(row, pairs))){
    for(p in pairs) row <- str_remove_all(row, p)
  }
  print(row)
  
  outvec <-c()
  counter=1
  for(l in nchar(row):1){
    outvec[counter] <- lut_closed[str_sub(row,l,l)]
    counter = counter+1
  }
  print(outvec)
  
  score <- 0
  for(ov in outvec){
    score <- score * 5
    score <- score+ lut_score[ov]
  }
  result2[i] <- score
  
}
median(result2)
length(result2)
sort(result2)[23]
