
Win_list = list()
sims = 25000

#Run number of designated simulations
for (i in 1:sims){

#Set up Data
#Choose order of contestants
SpellBee = data.frame(Contestants= c(1:10),
           ProbCorrect = seq(.90,.99,.01)) #%>%
          #arrange(desc(ProbCorrect))

#Keep running code until one contestant left
#Store winner in list
while(nrow(SpellBee)>1){
  
  for(j in 1:nrow(SpellBee)){
    
    if(as.numeric(j) <= nrow(SpellBee)){
      
    ProbC = SpellBee[j,]$ProbCorrect
    
    Accurate = as.numeric(sample(0:1,
                      1,
                      prob = c(1-ProbC,
                               ProbC)))
    
#Eliminate contestants who spell a word wrong
    if(Accurate==0){
      SpellBee = SpellBee %>%
        filter(ProbCorrect != ProbC)
      }
    }
  }
} 

Win_list[i] = as.numeric(SpellBee$Contestants)

}

Winners = data.frame(Winner=unlist(Win_list)) %>%
  group_by(Winner) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  mutate(Win_prop = count / sum(count))


