library(tidyverse)

psub <- read.csv('pitchByPitch_subset.csv', header = T)

psub_inplay <- psub[psub$pitchResult == 'InPlay',]

psub %>% group_by(groups) %>% summarize(inPlayPct = sum(pitchResult=='InPlay')/n(), 
                                        numInPlay = sum(pitchResult=='InPlay'), 
                                        numGroup = n())

View(psub_inplay %>% group_by(groups) %>% 
  summarize(
    hits = sum(result.event=='Single', result.event=='Double', result.event=='Triple', 
               result.event=='Home Run', result.event=='Catcher Interference'), 
    singles = sum(result.event=='Single'), 
    doubles = sum(result.event=='Double'), 
    triples = sum(result.event=='Triple'),
    hrs = sum(result.event=='Home Run'),
    sacs = sum(result.event=='Sac Fly', result.event=='Sac Bunt', result.event=='Sac Fly Double Play'), 
    outs = sum(result.event=='Pop Out', result.event=='Flyout', result.event=='Groundout', 
               result.event=='Lineout', result.event=='Fielders Choice', result.event=='Forceout', 
               result.event=='Grounded Into DP', result.event=='Field Error', result.event=='Fielders Choice Out', 
               result.event=='Double Play', result.event=='Bunt Groundout', result.event=='Bunt Pop Out', 
               result.event=='Bunt Lineout', result.event=='Triple Play'), 
    babip = (hits - hrs)/(hits + outs - hrs), 
    slg = (singles + 2*doubles + 3*triples + 4*hrs)/(hits + outs),
    avg = hits/n()) %>% select(groups, babip)
)



View(psub %>% group_by(groups) %>% 
     summarize(
       hits = sum(result.event=='Single', result.event=='Double', result.event=='Triple', 
                  result.event=='Home Run', result.event=='Catcher Interference'), 
       outs = sum(result.event=='Pop Out', result.event=='Flyout', result.event=='Groundout', 
                  result.event=='Lineout', result.event=='Fielders Choice', result.event=='Forceout', 
                  result.event=='Grounded Into DP', result.event=='Field Error', result.event=='Fielders Choice Out', 
                  result.event=='Double Play', result.event=='Bunt Groundout', result.event=='Bunt Pop Out', 
                  result.event=='Bunt Lineout', result.event=='Triple Play'), 
       abs = n(), 
       walks = sum(result.event=='Walk', result.event=='Hit by pitch'),
       sacs = sum(result.event=='Sac Fly', result.event=='Sac Bunt', result.event=='Sac Fly Double Play'), 
       avg = hits/(hits+outs-walks-sacs)
))



       
