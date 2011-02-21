module HEP.Physics.TTBar.TaskManager where

import System.IO

data Task = Task {
     inputfilename :: FilePath, 
     outputfilename :: FilePath,
     histname :: String, 
     canvasname :: String
  }
            
          
task_test = 
  Task "binary/test.binary"
       "test.pdf"
       "test"
       "test"


task_ttbar_SM = 
  Task "binary/ttbar_SM_10000_7TeV_pgs_events.binary"
       "ttbar_SM.pdf"
       "ttbar_SM"
       "ttbar_SM" 
       

task_ttbarj_SM = 
  Task "binary/ttbarj_SM_10000_7TeV_pgs_events.binary"
       "ttbarj_SM.pdf"
       "ttbarj_SM"
       "ttbarj_SM" 


task_ttbarjj_SM = 
  Task "binary/ttbarjj_SM_10000_7TeV_pgs_events.binary"
       "ttbarjj_SM.pdf"
       "ttbarjj_SM"
       "ttbarjj_SM" 


task_wprime_200 = 
  Task "binary/wprime200_10000_unweighted_events_parton.binary"
       "wprime200.pdf"
       "wprime200"
       "wprime200"

task_wprime_400 = 
  Task "binary/wprime400_10000_unweighted_events_parton.binary"
       "wprime400.pdf"
       "wprime400"
       "wprime400"



task_wp200 = 
  Task "binary/wp200_semilep_10000ev_unweighted_events_parton.binary"
       "wprime200.pdf"
       "wprime200"
       "wprime200"


taskset task = ( histname task, canvasname task, inputfilename task, outputfilename task ) 
