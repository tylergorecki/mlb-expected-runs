library(tidyverse)
set.seed(4996)

#source('expectedRuns_10.26.R')
######## simInning functions #########

pitchDist <- read.csv('newPitchDist.csv')
rownames(pitchDist) <- pitchDist[,1]
pitchDist = pitchDist[,-c(1)]
pitchDist[is.na(pitchDist)] <- 0
colnames(pitchDist) <- c('inplay', 'ball', 'strike', 'foul', 'hbp')

hitDist <- read.csv('newHitDist.csv')
rownames(hitDist) <- hitDist[,1]
hitDist = hitDist[,-c(1)]
hitDist[is.na(hitDist)] <- 0
colnames(hitDist) <- c('single', 'double', 'triple', 'homerun', 'error', 
                       'popout', 'flyout', 'lineout', 'groundout')

reset_params <- function() {
  balls <<- 0
  strikes <<- 0
  outs <<- 0
  runs <<- 0
  baserunners <<- c(0,0,0)
  # lineup <<- replicate(3, c('contact', 'middle', 'power'))
  hitterNum <<- 1
  newHitter <<- T
}

lineup <- lineups[[127]]
reset_params()
set.seed(2)
simInning()

simInning <- function() {
  
  Sys.sleep(.5)
  #hitter <- hitting[hitterNumber]
  
  print('--------------------------------------------')
  Sys.sleep(2)
  
  if (newHitter == T) {
    hitter <<- lineup[hitterNum]
    print(paste('NEW HITTER: ', hitterNum, hitter))
    Sys.sleep(.5)
    print(paste('Current Runs: ', runs))
    Sys.sleep(.5)
    print(paste('Baserunners: ', baserunners[1], baserunners[2], baserunners[3]))
    Sys.sleep(.5)
    print(paste('Outs: ', outs))
    cat('\n')
    Sys.sleep(1)
    hitterNum <<- ifelse(hitterNum == 9, 1, hitterNum + 1)
  }
  
  newHitter <<- F
  
  print(paste('Count: ', balls, '-', strikes))
  Sys.sleep(1)
  
  # function will return one of five outcomes: ball, strike, foul, hbp, inplay
  outcome <- simOutcome()
  
  print(paste('Pitch Outcome: ', outcome))
  Sys.sleep(.5)
  #print(baserunners)
  
  # change function parameters based on outcome
  if (outcome == 'ball') {
    #print(paste('ball', balls, strikes))
    balls <<- balls + 1
    if (balls == 4) {
      print('WALK')
      walk()
      newHitter <<- T
      balls <<- strikes <<- 0
      #print('walk')
      #print(baserunners)
    }
  }
  
  if (outcome == 'strike') { 
    #print(paste('strike', balls, strikes))
    strikes <<- strikes + 1
    if (strikes == 3) {
      print('STRIKEOUT')
      outs <<- outs + 1
      balls <<- strikes <<- 0
      #print('strikeout')
      #print(baserunners)
      newHitter <<- T
    }
  }
  
  if (outcome == 'foul') { 
    #print(paste('foul', balls, strikes))
    strikes <<- ifelse(strikes == 2, 2, strikes + 1)
  }
  
  if (outcome == 'hbp') {
    #print(paste('hbp', balls, strikes))
    advanceBaserunners(1)
    balls <<- strikes <<- 0
    #print('hbp')
    #print('END OF AT BAT')
    #print(baserunners)
    newHitter <<- T
  }
  
  if (outcome == 'inplay') {
    Sys.sleep(1)
    #print(paste('inplay', balls, strikes))
    play = inplay()
    newHitter <<- T
    
    print(paste('Hit Result: ', play))
    Sys.sleep(.5)
    
    #print(baserunners)
    
    if (play == 'error') {
      # 65% single, 25% double, 8% triple, 2% homerun
      eplay <- sample(c('single', 'double', 'triple', 'homerun'), size = 1, prob = c(.65,.25,.08,.02))
      if (eplay == 'single') {
        single()
        balls <<- strikes <<- 0
      } else if (eplay == 'double') {
        double()
        balls <<- strikes <<- 0
      } else if (eplay == 'triple') {
        triple()
        balls <<- strikes <<- 0
      } else if (eplay == 'homerun') {
        homerun()
        balls <<- strikes <<- 0
      }
    }
    
    if (play == 'single') {
      single()
      balls <<- strikes <<- 0
    } 
    else if (play == 'double') {
      double()
      balls <<- strikes <<- 0
    } 
    else if (play == 'triple') {
      triple()
      balls <<- strikes <<- 0
    } 
    else if (play == 'homerun') {
      homerun()
      balls <<- strikes <<- 0
    }
    else if (play == 'popout') {
      balls <<- strikes <<- 0
      outs <<- outs + 1
    }
    else if (play == 'flyout') {
      flyout()
    }
    else if (play == 'lineout') {
      balls <<- strikes <<- 0
      outs <<- outs + 1
    }
    else if (play == 'groundout') {
      groundout()
    }
  }
  
  # print(runs)
  if (outs < 3) { return(simInning()) }
  
  Sys.sleep(1)
  print(paste('Final Runs Scored: ', runs))
  #return(runs)
}

simOutcome <- function() {
  row = paste(hitter, '_', balls, '_', strikes, sep = '')
  countProb = pitchDist[row,]
  
  print(data.frame(countProb), row.names=F)
  Sys.sleep(1)
  
  result = sample(colnames(pitchDist), size = 1, prob = countProb, replace = TRUE)
  return(result)
}

inplay <- function() {
  row = paste(hitter, '_', balls, '_', strikes, sep = '')
  hitProb = hitDist[row,]
  
  # print(row)
  # print(colnames(hitDist))
  # print(hitProb)
  
  print(data.frame(hitProb), row.names=F)
  Sys.sleep(1)
  
  result = sample(colnames(hitDist), size = 1, prob = hitProb, replace = TRUE)
  return(result)
}

advanceBaserunners <- function(num) {
  #print(num)
  #print(baserunners)
  if (num == 0) { return() }
  
  if (num == 1) {
    if (baserunners[3] == 1) { 
      runs <<- runs + 1
      baserunners[3] <<- 0
    }
    if (baserunners[2] == 1) { 
      baserunners[3] <<- 1
      baserunners[2] <<- 0
    }
    if (baserunners[1] == 1) { 
      baserunners[2] <<- 1
      baserunners[1] <<- 0
    }
    baserunners[1] <<- 1
  }
  
  if (num == 2) {
    if (baserunners[3] == 1) {
      runs <<- runs + 1
      baserunners[3] <<- 0
    }
    if (baserunners[2] == 1) {
      runs <<- runs + 1
      baserunners[2] <<- 0
    }
    if (baserunners[1] == 1) {
      baserunners[3] <<- 1
      baserunners[1] <<- 0
    }
    baserunners[2] <<- 1
  }
  
  if (num == 3) {
    if (baserunners[3] == 1) {
      runs <<- runs + 1
      baserunners[3] <<- 0
    }
    if (baserunners[2] == 1) {
      runs <<- runs + 1
      baserunners[2] <<- 0
    }
    if (baserunners[1] == 1) {
      runs <<- runs + 1
      baserunners[1] <<- 0
    }
    baserunners[3] <<- 1
  }
  
  if (num == 4) {
    runs <<- 1 + sum(baserunners)
    baserunners <<- c(0,0,0)
  }
  
  # print(runs)
  return()
}

walk <- function() {
  runs <<- runs + ifelse(baserunners[1] + baserunners[2] + baserunners[3] == 3, 1, 0)
  baserunners[3] <<- ifelse(baserunners[1] + baserunners[2] == 2, 1, baserunners[3])
  baserunners[2] <<- ifelse(baserunners[1] == 1, 1, baserunners[2])
  baserunners[1] <<- 1
}

single <- function() {
  #print(baserunners)
  # 15% all, 40% base, 5% lead out, 5% trail out, 35% lead 
  runners <- sample(c('all', 'base', 'leadout', 'trailout', 'leadscores'), size = 1, prob = c(.15,.4,.05,.05,.6))
  if (baserunners[3] == 1) { 
    runs <<- runs + 1
    baserunners[3] <<- 0
  }
  if (baserunners[2] == 1) { 
    if (runners == 'all' | runners == 'leadscores') {
      runs <<- runs + 1
      baserunners[3] <<- 0
      baserunners[2] <<- 0
    } else if (runners == 'trailout') {
      runs <<- runs + 1
      outs <<- outs + 1
      baserunners[3] <<- 0
      baserunners[2] <<- 0
    } else if (runners == 'base') {
      baserunners[3] <<- 1
      baserunners[2] <<- 0
    } else if (runners == 'leadout') {
      outs <<- outs + 1
      baserunners[3] <<- 0
      baserunners[2] <<- 0
    }
  }
  if (baserunners[1] == 1) {
    if (runners == 'all') {
      baserunners[3] <<- 1
    } else if (runners == 'trailout') {
      outs <<- outs + 1
      baserunners[3] <<- 0
      baserunners[2] <<- 0
    } else if (runners == 'leadout') {
      outs <<- outs + 1
      baserunners[2] <<- 1
    } else if (runners == 'base' | runners == 'leadscores') {
      baserunners[2] <<- 1
    }
  }
  baserunners[1] <<- 1
  # print(runners)
  # print(runs)
  # print(baserunners)
}

double <- function() {
  #print(baserunners)
  # 35% firstscores, 60% base, 5% out
  runners <- sample(c('first', 'base', 'out'), size = 1, prob = c(.35,.6,.05))
  if (baserunners[3] == 1) {
    runs <<- runs + 1
    baserunners[3] <<- 0
  }
  if (baserunners[2] == 1) {
    runs <<- runs + 1
    baserunners[2] <<- 0
  }
  if (baserunners[1] == 1) {
    if (runners == 'first') {
      runs <<- runs + 1
      baserunners[1] <<- 0
    } else if (runners == 'base') {
      baserunners[3] <<- 1
      baserunners[1] <<- 0
    } else if (runners == 'out') {
      outs <<- outs + 1
      baserunners[1] <<- 0
    }
  }
  baserunners[2] <<- 1
  #print(runners)
  #print(runs)
  #print(baserunners)
}

triple <- function() {
  if (baserunners[3] == 1) {
    runs <<- runs + 1
    baserunners[3] <<- 0
  }
  if (baserunners[2] == 1) {
    runs <<- runs + 1
    baserunners[2] <<- 0
  }
  if (baserunners[1] == 1) {
    runs <<- runs + 1
    baserunners[1] <<- 0
  }
  baserunners[3] <<- 1
}

homerun <- function() {
  runs <<- 1 + sum(baserunners)
  baserunners <<- c(0,0,0)
}

flyout <- function() {
  
  if (baserunners[2] == 1 & baserunners[3] == 1) {
    
    result <- sample(c('both', 'lead', 'leadout', 'trailout', 'stay'), size = 1, prob = c(.2, .5, .05, .05, .2))
    
    if (outs < 2) {
      if (result == 'both') {
        runs <<- runs + 1
        baserunners[2] <<- 0
        #print('both')
      } else if (result == 'lead') {
        runs <<- runs + 1
        baserunners[3] <<- 0
        #print('lead')
      } else if (result == 'leadout') {
        outs <<- outs + 1
        baserunners[2] <<- 0
        #print('leadout')
      } else if (result == 'trailout') {
        runs <<- runs + 1
        outs <<- outs + 1
        baserunners[2] <<- 0
        baserunners[3] <<- 0
        #print('trailout')
      }
    }
    
    balls <<- strikes <<- 0
    outs <<- outs + 1
    
  } 
  
  else if (baserunners[2] == 1 & baserunners[3] == 0) {
    
    result <- sample(c('advance', 'stay', 'out'), size = 1, prob = c(.2, .75, .05))
    
    if (outs < 2) {
      if (result == 'advance') {
        baserunners[3] <<- 1
        baserunners[2] <<- 0
      } else if (result == 'out') {
        outs <<- outs + 1
        baserunners[2] <<- 0
      }
    }
    
    balls <<- strikes <<- 0
    outs <<- outs + 1
    
  } 
  
  else if (baserunners[2] == 0 & baserunners[3] == 1) {
    
    result <- sample(c('advance', 'stay', 'out'), size = 1, prob = c(.65, .30, .05))
    
    if (outs < 2) {
      if (result == 'advance') {
        runs <<- runs + 1
        baserunners[3] <<- 0
      } else if (result == 'out') {
        outs <<- outs + 1
        baserunners[3] <<- 0
      }
    }
    
    balls <<- strikes <<- 0
    outs <<- outs + 1
    
  } 
  
  else {
    
    balls <<- strikes <<- 0
    outs <<- outs + 1
    
  }
  
}

groundout <- function() {
  
  # treating every situation as infield back
  # might only do double play or guys advance, just changing chance of dp
  if (outs < 2) {
    
    if (baserunners[1] == 1 & baserunners[2] == 1 & baserunners[3] == 1) {
      result <- sample(c('dp', 'leadout', 'hitout'), size = 1, prob = c(.4, .45, .15))
      
      if (result == 'dp') {
        if (outs == 0) {
          runs <<- runs + 1
          baserunners[1] <<- 0
          baserunners[2] <<- 0
        }
        outs <<- outs + 2
      }
      else if (result == 'leadout') {
        runs <<- runs + 1
        baserunners[2] <<- 0
        outs <<- outs + 1
      }
      else if (result == 'hitout') {
        runs <<- runs + 1
        baserunners[1] <<- 0
        outs <<- outs + 1
      }
    }
    
    else if (baserunners[1] == 1 & baserunners[2] == 1 & baserunners[3] == 0) {
      result <- sample(c('dp', 'leadout', 'hitout'), size = 1, prob = c(.4, .45, .15))
      
      if (result == 'dp') {
        baserunners[1] <<- 0
        baserunners[2] <<- 0
        baserunners[3] <<- 1
        outs <<- outs + 2
      }
      else if (result == 'leadout') {
        baserunners[2] <<- 0
        baserunners[3] <<- 1
        outs <<- outs + 1
      }
      else if (result == 'hitout') {
        baserunners[1] <<- 0
        baserunners[3] <<- 1
        outs <<- outs + 1
      }
    }
    
    else if (baserunners[1] == 1 & baserunners[2] == 0 & baserunners[3] == 1) {
      result <- sample(c('dp', 'leadout', 'hitout'), size = 1, prob = c(.4, .45, .15))
      
      if (result == 'dp') {
        if (outs == 0) {
          runs <<- runs + 1
          baserunners[1] <<- 0
          baserunners[3] <<- 0
        }
        outs <<- outs + 2
      }
      else if (result == 'leadout') {
        runs <<- runs + 1
        baserunners[3] <<- 0
        outs <<- outs + 1
      }
      else if (result == 'hitout') {
        runs <<- runs + 1
        baserunners <<- c(0,1,0)
        outs <<- outs + 1
      }
    }
    
    else if (baserunners[1] == 1 & baserunners[2] == 0 & baserunners[3] == 0) {
      result <- sample(c('dp', 'leadout', 'hitout'), size = 1, prob = c(.4, .45, .15))
      
      if (result == 'dp') {
        baserunners[1] <<- 0
        outs <<- outs + 2
      }
      else if (result == 'leadout') {
        outs <<- outs + 1
      }
      else if (result == 'hitout') {
        baserunners[1] <<- 0
        baserunners[2] <<- 1
        outs <<- outs + 1
      }
    }
    
    else if (baserunners[1] == 0 & baserunners[2] == 1 & baserunners[3] == 1) {
      # hitter out regardless
      
      runs <<- runs + 1
      baserunners <<- c(1,0,1)
      outs <<- outs + 1
    }
    
    else if (baserunners[1] == 0 & baserunners[2] == 1 & baserunners[3] == 0) {
      # hitter out regardless
      
      baserunners <<- c(0,0,1)
      outs <<- outs + 1
    }
    
    else if (baserunners[1] == 0 & baserunners[2] == 0 & baserunners[3] == 1) {
      # hitter out regardless
      
      runs <<- runs + 1
      baserunners <<- c(0,0,0)
      outs <<- outs + 1
    }
    
    else if (baserunners[1] == 0 & baserunners[2] == 0 & baserunners[3] == 0) {
      # hitter out, nothing else changes
      
      outs <<- outs + 1
    }
    
  }
  
  else {
    outs <<- outs + 1
  }
  
  balls <<- strikes <<- 0
}

#################################################

#saveRDS(lineups3c4m2p, 'lineups3c4m2p')

lineuptype <- 'lineups252'
lineups <- readRDS(lineuptype)

# change for each run through
num_lineups <- length(lineups)
num_iterations <- 1000

final_df <- data.frame(matrix(0, nrow = num_lineups*num_iterations, ncol = 30))
colnames(final_df) <- c('l1','l2','l3','l4','l5','l6','l7','l8','l9',
                       'simNum','totalRuns','r1','r2','r3','r4','r5','r6','r7',
                       'r8','r9','h1','h2','h3','h4','h5','h6','h7','h8','h9','h10')

start_time <- system.time({
  
  #lineup <- replicate(9,'contact')
  #lineup <<- c('contact','middle','power','power','middle','middle','contact','power','contact')
  
  #lineups <- lineups252[1:num_lineups]
  x <- 0
  
  for (z in 1:100) {
    #print(z)
    #cat('Lineup:', z, '\r')
    lineup <- lineups[[z]]
    
    low <- x + 1
    high <- x + num_iterations
    
    for (i in (low:high)) {
      # set lineup column
      final_df[i,1:9] = lineup
      
      # set simulation number
      modIter = i %% num_iterations
      simNum = ifelse(modIter == 0, 1000, modIter)
      final_df[i,'simNum'] = simNum
      cat('Lineup:', z, ' - Iteration:', simNum, '\r')
      
      # lineup <<- replicate(3, c('contact', 'middle', 'power'))
      # lineup <<- replicate(9,'power')
      
      hitterNum <<- 1
      
      for (j in 1:9) {
        # set inning/hitter strings
        inn = paste('r',j,sep = '')
        hitter = paste('h',j,sep = '')
        
        # get runs per inning
        balls <<- 0
        strikes <<- 0
        outs <<- 0
        runs <<- 0
        baserunners <<- c(0,0,0)
        newHitter <<- T
        
        # set start hitter value
        final_df[i,hitter] = hitterNum
        
        # calculate inning runs
        totalRuns = simInning()
        
        # set inning run value
        final_df[i,inn] = totalRuns
      }
      
      final_df[i,paste('h',10,sep = '')] = hitterNum + 1
      
      # sum total runs
      final_df[i,'totalRuns'] = sum(final_df[i,'r1'], final_df[i,'r2'], final_df[i,'r3'], 
                                    final_df[i,'r4'], final_df[i,'r5'], final_df[i,'r6'], 
                                    final_df[i,'r7'], final_df[i,'r8'], final_df[i,'r9'])
      
      #print(i)
    }
    
    x <<- x + num_iterations
    #print(x)
  }
  
  final_df <- final_df %>%
    unite(lineup, l1:l9, sep = ",")
  
  summary <- final_df %>% 
    group_by(lineup) %>% 
    summarize(mean_runs_per_inning = round(mean(totalRuns)/9, 3)) %>% 
    arrange(-mean_runs_per_inning)
  
})

elapsed_time <- as.numeric(start_time['elapsed'])
cat('Elapsed time: ', elapsed_time, ' seconds')

# MUST CHANGE SUBFOLDER DEPENDING ON LINEUP
full.path <- paste('analysisOutputs/', lineuptype, '/fulldf.', 
                   num_lineups, '.', num_iterations, '.csv', sep = '')

sum.path <- paste('analysisOutputs/', lineuptype, '/summary.', 
                  num_lineups, '.', num_iterations, '.csv', sep = '')

write.csv(final_df, full.path, row.names = F)
write.csv(summary, sum.path, row.names = F)

