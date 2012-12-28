;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paper: Both Selfish and Other-Regarding Preferences Result from Natural Selection
;;; by:    Thomas Grund and Dirk Helbing
;;; Code for Simulation written by Thomas Grund
;;; This version includes additional comments and remarks by Christian Waloszek and Stefan Rustler
;;; Version 1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


extensions[array]
breed [agents agent]
;;;;;;;;;;;;;;;;; Global variables ;;;;;;;;;;;;;;;;;;;;
agents-own [ prob-behavior-change prob-reproduction payoffC payoffD strategy fitness otherregarding c-neighbors d-neighbors mutated]
globals [ hist-other P R coop-share other-all mutated-offsprings offspring cooperators defectors num-cooperators num-defectors other-cooperators other-defectors fitness-cooperators fitness-defectors mutated-coops mutated-defs]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to setup
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Initialize various Globals
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  clear-all
  set P 0
  set R 1
  set other-all 0
  set coop-share 0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Setup Patches and Agents
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ask patches [
    set pcolor white
  ]
  ask n-of (ceiling(population * (max-pxcor + 1) * (max-pycor + 1))) patches [
    sprout-agents 1 [
      set shape "square"
      set color red
      set strategy 0                ;;; set all Agents Strategies to "Defecting" (= Strategy = 0)
      set otherregarding 0          ;;; in Paper: = rho = "firendliness" = "degree of other-regarding-preferences" from agent
      set mutated false             ;;; boolean : "is agent mutated?" = YES / NO
      set prob-behavior-change 0    ;;; in Paper: = m√ºh = random mutation probability, that any agent turns from a cooperator into a defector or vice versa 
    ]
  ]
  ask agents [
    set c-neighbors count ((turtles-on neighbors) with [strategy = 1])
    set d-neighbors count ((turtles-on neighbors) with [strategy = 0])
  ]
  
  set hist-other array:from-list n-values 100 [0]
  array:set hist-other 0 (count agents)
  reset-ticks
end  ;---------------------------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Go! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  tick
  ask-concurrent agents [ update-behavior]
  strategy-update-stage
  game-stage
  reproduction-stage
  update-variables
  my-update-plots
end  ;---------------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; General Procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; RANDOM STRATEGY MUTATION of AGENTS
  ;; Take Rationality of Agents into account:
  ;; with "prob-behavior-change"-percent, the agent behaves rationaly
  ;; if K = 0 : all egnts are perfectly rational
  ;; if K > 0 : some agents do not follow best response rule, but behave sub-optimal (smearing of probability according to Fermi-Function
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
to update-behavior
  if (random-float 1 < prob-behavior-change) [
    set strategy abs(strategy - 1)
    ifelse strategy = 1 [
      set color blue
    ]
    [
      set color red
    ]
  ]
end  ;---------------------------------------------------------------------------------------



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CALCULATE AGENTS UTILITY
  ;; utility of agents depends on own Pay-off and depending on his "friendliness" also on the Pay-offs of his neighbors
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;; U(C) = get-payoffC = Utility of Agent if he cooperates  = in Paper: U(C) = d[....
to-report get-payoffC 
    let ego_payoffC ( c-neighbors * R + d-neighbors * S)
    let alters_payoffC (d-neighbors * T + c-neighbors * R)    
    report  (1 - otherregarding) * ego_payoffC + otherregarding * alters_payoffC
end  ;--------------------------------------------------------------------------------------- 

  ;;;;; U(D) = get-payoffD = Utility of Agent if he defects = in Paper: U(D) = dP + c.......
to-report get-payoffD
    let ego_payoffD ( c-neighbors * T + d-neighbors * P)   
    let alters_payoffD (d-neighbors * P + c-neighbors * S)   
    report  (1 - otherregarding) * ego_payoffD + otherregarding * alters_payoffD   
end  ;--------------------------------------------------------------------------------------- 
   
   


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; STRATEGY UPDATING PROCESS
  ;; depending on his successrate in the last round, agent changes or keeps his strategy
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to strategy-update-stage
  ;; every agent cares about the situation around him at the end of the last round 
  ask-concurrent agents [ 
    set c-neighbors count (turtles-on neighbors) with [strategy = 1]
    set d-neighbors count (turtles-on neighbors) with [strategy = 0]
    ; show c-neighbors
    ; show d-neighbors 
    set payoffC get-payoffC ;; in Paper = U(C)
    set payoffD get-payoffD ;; in Paper = U(D)
  ]  
  ask-concurrent agents [
    ifelse (strategy = 1) [
      set prob-behavior-change   ( 1 / ( 1 + (exp((payoffC - payoffD)/ K))))
    ]  
    [
      set prob-behavior-change  ( 1 / ( 1 + (exp((payoffD - payoffC) / K))))
    ]
  ]
end  ;---------------------------------------------------------------------------------------



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GAME-STAGE
  ;; update fitness of agents, one by one
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to game-stage
  ask-concurrent agents [
    update-fitness  
  ]
end  ;--------------------------------------------------------------------------------------- 



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; REPRODUCTION STAGE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to reproduction-stage
  set mutated-offsprings 0
  set offspring 0
  ask agents [
    if death-rate > random-float 1 [
      if (strategy = 1) [
        ask turtles-on neighbors [
          set c-neighbors (c-neighbors - 1)
          update-fitness
        ]
      ]
      if (strategy = 0)[
        ask turtles-on neighbors [
          set d-neighbors (d-neighbors - 1)
          update-fitness
        ]
      ]
      set offspring offspring + 1
      let temp ceiling(otherregarding / 0.01) ;; "ceiling number"  = Reports the smallest integer greater than or equal to number.
      ;;                                          --------------------------------- ?????????????? Whats going on here.
      if temp > 0 [
        set temp temp - 1
      ]
      let value-to-update array:item hist-other temp 
      array:set hist-other temp (value-to-update - 1)
      die
    ]
  ]
  let parents select-with-prob offspring 
  foreach parents [
    ask ?1 [
      produce-offspring
    ]
  ]  
  ;;show (word "Off: " offspring "  Mut: " mutated-offsprings)
end  ;--------------------------------------------------------------------------------------- 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; FITNESS RULE
  ;; According to the fitness-rule = "(payoff+8*abs(S))" used in the paper 
  ;; returns a list with guys to be hatched
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to-report select-with-prob [ num ]
  let num-found 0
  let index 0
  let running 0
  let a-list sort agents
  let ran sort (n-values num [random sum [(fitness + 8 * abs(S))] of agents])
  let found-list (list)
  while [ num-found < num ] [
    while [running < item num-found ran] [
        set running running + [(fitness + 8 * abs(S))] of item index a-list
        set index index + 1
    ]
    if index > 0 [ set index index - 1]
    let one-to-add item index a-list
    set found-list fput one-to-add found-list
    set num-found num-found + 1
  ] 
  report found-list
end  ;--------------------------------------------------------------------------------------- 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; FITNESS UPDATE & CALCULATION
  ;; Calculate or update the fitness of Agents  (his probability for Reproduction)         
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to update-fitness 
  set c-neighbors count (agents-on neighbors) with [ strategy = 1]
  set d-neighbors count (agents-on neighbors) with [ strategy = 0]
  if strategy = 0 [
    set fitness  ( c-neighbors * T + d-neighbors * P)
  ]
  if strategy = 1 [
    set fitness ( c-neighbors * R + d-neighbors * S)
  ]
end  ;--------------------------------------------------------------------------------------- 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; OFFSPRING PRODCUCTION
  ;; depending on the probability for localreproduction chosen in the interface
  ;; offspring hatches either close to the parents site or on a randomly chosen lattice patch
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to produce-offspring
   ifelse random-float 1 > localreproduction [
     ;;;;;;;; hatch offspring on any randomly chosen free patch ;;;;;;;;;;;;;;;;;;;;;;;;;;
     hatch 1 [
       addError
       move-to one-of patches with [count turtles-here = 0 ]
       update-fitness
       let temp ceiling(otherregarding / 0.01)
       if temp > 0 [
         set temp temp - 1
       ]
       let value-to-update array:item hist-other temp
       array:set hist-other temp (value-to-update + 1)
     ]
   ]  
   [
     ;;;;;;;; hatch offspring on a free patch closest to parent ;;;;;;;;;;;;;;;;;;;;;;;;;;
     hatch 1 [
       addError
       let empty-patches patches with [ count turtles-here = 0 ]
       move-to (one-of (empty-patches with-min [ distance myself ]))
       update-fitness
       let temp ceiling(otherregarding / 0.01)
       if temp > 0 [
         set temp temp - 1
       ]
       let value-to-update array:item hist-other temp
       array:set hist-other temp (value-to-update + 1)
     ]
   ]
end  ;--------------------------------------------------------------------------------------- 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; OFFSPRING STRATEGY MUTATION
  ;; OFFSPRING of AGENTS inherits friendliness (=otherregarding) of parent with certain probability
  ;; the exact value of friendliness is subject to random mutation
  ;; the mutation probability (="otherregarding-mutation"-probability = in Paper: mÃ¼h) can be set in the interface, allowing the following options:
  ;; "random-reset" = 100% "otherregarding-mutation"-probability 
  ;; "90%-down-reset" = 90% of "otherregarding-mutation"-probability 
  ;; "80%-down-reset" = 80% of "otherregarding-mutation"-probability 
  ;; "50%-down-reset" = 50% of "otherregarding-mutation"-probability 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to addError
  ifelse random-float 1 < otherregarding-mutation [
    if other-error = "random-reset" [
      set otherregarding random-float 1
    ]
    if other-error = "90%-down-reset" [
      ifelse random-float 1 < 0.9 [
        set otherregarding random-float otherregarding
      ]
      [ set otherregarding (otherregarding + random-float (1 - otherregarding)) ]
    ]
    if other-error = "80%-down-reset" [
      ifelse random-float 1 < 0.8 [
        set otherregarding random-float otherregarding
      ]
      [ set otherregarding (otherregarding + random-float (1 - otherregarding)) ]
    ]
    if other-error = "50%-down-reset" [
      ifelse random-float 1 < 0.5 [
        set otherregarding random-float otherregarding
      ]
      [ set otherregarding (otherregarding + random-float (1 - otherregarding)) ]
    ]
    set mutated true
    set mutated-offsprings mutated-offsprings + 1
  ]
  [
    set mutated false
  ] 
end  ;--------------------------------------------------------------------------------------- 



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; UPDATE VARIABLES
  ;; for easier Analysis in Behavior Space, define certain new variables
  ;; EX.: set "mean [ fitness ] of cooperators" = "fitness-cooperators"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to update-variables
  
  set cooperators agents with [ strategy = 1 ]
  set defectors agents with [ strategy = 0] 
  set num-cooperators count cooperators
  set num-defectors count defectors
  
  set other-all mean [otherregarding] of agents
  set coop-share (count cooperators / (count agents))

  
  if num-cooperators > 0 [
    set other-cooperators mean [ otherregarding] of cooperators
    set fitness-cooperators mean [ fitness] of cooperators 
    set mutated-coops count cooperators with [ mutated = true ]
  ]
  if num-defectors > 0 [
    set other-defectors mean [ otherregarding] of defectors
    set fitness-defectors mean [ fitness] of defectors 
    set mutated-defs count cooperators with [ mutated = true ]
  ]
end  ;--------------------------------------------------------------------------------------- 
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; UPDATE PLOTS
  ;; 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to my-update-plots
  ;; PLOT Average Friendliness ρ of Cooperators and Defectors
  set-current-plot "ρ Cooperators + Defectors"
  plot mean [otherregarding] of agents  
  set cooperators agents with [ strategy = 1 ]
  set defectors agents with [ strategy = 0]
  ;; 
  set-current-plot "share of cooperators"
  plot (count cooperators / (count agents))
  
  if count cooperators > 0 [
    ;; PLOT Average Friendliness ρ of Cooperators
    set-current-plot "ρ Cooperators"
    plot mean [ otherregarding] of cooperators
    ;; PLOT Average Fitness of Cooperators (= Reproduction Probability of Cooperators)
    set-current-plot "fitness cooperators"
    plot mean [ fitness ] of cooperators
    ;; PLOT Average Fitness of Cooperators and Defectors in one Plot
    set-current-plot "fitness"
    set-current-plot-pen "cooperators"
    plot mean [ fitness ] of cooperators
    ;; PLOT Average Number of mutated Cooperators
    set-current-plot "mutated cooperators"
    plot mutated-coops / count cooperators
  ]
  
  if count defectors > 0 [
    ;; PLOT Average Friendliness ρ of Defectors
    set-current-plot "ρ Defectors"
    plot mean [ otherregarding ] of defectors
    ;; PLOT Average Fitness of Defectors (= Reproduction Probability of Defectors)
    set-current-plot "fitness defectors"
    plot mean [ fitness ] of defectors
    ;; PLOT Average Fitness of Cooperators and Defectors in one Plot
    set-current-plot "fitness"
    set-current-plot-pen "defectors"
    plot mean [ fitness ] of defectors
    ;; PLOT Average Number of mutated Defectors
    set-current-plot "mutated defectors"
    plot mutated-defs / count defectors 
  ]

  


  ;;plot mean [ fitness] of agents with [ strategy = 1]

  
end  ;---------------------------------------------------------------------------------------  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes made to Version from Thomas Grund
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; in Vers 1.0:
;;---------------------------------------------------------------------------------------  
;; deleted variables ( had no purpose and thus were deleted from code ):
;;           - needs-updating false
;;           - payoff-current
;;           - payoff-alternative
;;           - reproduction
;;
;; in Vers 2.0:
;;---------------------------------------------------------------------------------------  
;; deleted variables
;;           - C-above-50
;;           - other-above-25
;; deleted procedures ( had no purpose and thus were deleted from code ):
;;           - to update-ngh-info --- UPDATE the AGENT's NEIGHBOURHOOD INFORMATION
;;           - to update-color
;;
;; in Vers. 3.0
;;---------------------------------------------------------------------------------------  
;; in procedure "to calculate-reproduction-prob" changed the used fitness rule from "exp(payoff/L)" to "(fitness + 8 * abs(S))"
;;
;; in Vers. 3.5
;; added further comments to "Info"-panel regarding the different Parameters and Variables used in the Model
;;
;; in Vers. 4.0
;;---------------------------------------------------------------------------------------  
;; ->  FITNESS RULE & "select-with-prob [ num ]"
;;     To clean up the code, the "fitness rules", and the respective Chooser in the interface, which were not applied in the Paper were deleted.
;;     As Parameter "L" only in fitness rule applied, also deleted respective slider in Interface
;;     If desired they are still of any further interst, they can be found and copied from in previous versions.
;;
;; in Vers. 5.0
;;---------------------------------------------------------------------------------------  
;; updated information in "Info"-panel
;;
;; deleted variables:
;;          - coop-avg
;;          - other-avg
;;          - coop-sum
;;          - other-sum
;;          - wait-measuring-until (defined by slider in interface)
;; deleted procedures ( had no purpose and thus were deleted from code ):
;;          - to go-100
;;          - to calculate-avg-scores
;;          - in: to update-variables deleted:   if ticks > wait-measuring-until [
;;                                                                                set coop-sum coop-sum + coop-share
;;                                                                                set other-sum other-sum + other-all
;;                                                                                ]
;;
;; in Vers. 5.5
;; added further comments in Interface and rearranged Boxes and Windows
;;
;; in Vers. 6.0
;;---------------------------------------------------------------------------------------  
;; updated comments in Interface and Code, rearranged Boxes and Windows in Interface
;; procedure "UPDATE VARIABLES" mainly intended to have variables which can be easier used in BehaviorSpace.
;; Variables of Interest for Graphs in Paper:
;;  for Figure 1 (exp_fig1):
;;    - coop-share = (count cooperators / (count agents))
;;    - other-all = mean [ otherregarding ] = Mean Friendliness rho
;;    - fitness-cooperators = mean [ fitness ] of cooperators
;;    - fitness-defectors = mean [ fitness ] of defectors
;:; with precision 1.23456789 3 => 1.235
;;
;; deleted procedure:
;;          - to calculate-reproduction-prob
;;          - prob-reproduction
;; deleted plot:
;;          - "reproduction probability"
;;
;; in Vers. 7.0
;;---------------------------------------------------------------------------------------  
@#$#@#$#@
GRAPHICS-WINDOW
24
86
359
442
-1
-1
10.833333333333334
1
10
1
1
1
0
1
1
1
0
29
0
29
0
0
1
ticks
30.0

SLIDER
379
86
551
119
population
population
0
1
0.6
.01
1
NIL
HORIZONTAL

BUTTON
24
18
90
51
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
98
18
161
51
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
379
203
551
236
localreproduction
localreproduction
0
1
1
.05
1
NIL
HORIZONTAL

SLIDER
590
283
800
316
otherregarding-mutation
otherregarding-mutation
0
1
0.05
.001
1
NIL
HORIZONTAL

PLOT
24
446
360
596
share of cooperators
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

MONITOR
814
368
922
413
other cooperators
mean [ otherregarding] of agents with [strategy = 1]
5
1
11

MONITOR
923
368
1018
413
other defectors
mean [ otherregarding] of agents with [strategy = 0]
5
1
11

PLOT
814
442
1014
592
ρ Cooperators
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1026
442
1226
592
ρ Defectors
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1239
442
1439
592
fitness cooperators
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
1450
442
1650
592
fitness defectors
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

MONITOR
183
19
277
64
NIL
count agents\n
17
1
11

SLIDER
379
143
551
176
death-rate
death-rate
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
588
86
760
119
T
T
0
2
1.1
.01
1
NIL
HORIZONTAL

SLIDER
588
123
760
156
S
S
-1
1
-1
0.1
1
NIL
HORIZONTAL

PLOT
1240
210
1599
419
fitness
ticks
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"defectors" 1.0 0 -2674135 true "" ""
"cooperators" 1.0 0 -13345367 true "" ""

MONITOR
281
19
344
64
NIL
offspring\n
17
1
11

PLOT
378
445
578
595
mutated cooperators
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
588
445
788
595
mutated defectors
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
378
326
550
359
K
K
0
1
0.01
0.01
1
NIL
HORIZONTAL

PLOT
813
216
1018
366
ρ Cooperators + Defectors
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

CHOOSER
590
318
709
363
other-error
other-error
"random-reset" "90%-down-reset" "80%-down-reset" "50%-down-reset"
2

TEXTBOX
383
68
541
86
Occupancy of patches by Agents
10
0.0
1

TEXTBOX
388
127
477
145
β - Death Rate
10
0.0
1

TEXTBOX
384
188
564
206
ν - Local Reproduction Probability
10
0.0
1

TEXTBOX
594
165
773
281
μ - Mutation-Rate of Friendliness ρ\nfriendliness ρ_parent is inherited to offspring, but with mutation probability μ, ρ_child is set to\n⋄ 0 ≤ ρ_child ≤ ρ_parent\n   with Probability P = 0.8\n⋄ ρ_parent ≤ ρ_child ≤ 1\n   with Probability P = 0.2\n(if other-error 80% is selected)
10
0.0
1

TEXTBOX
596
58
746
84
Prisoner Dilemma Pay-Offs\nR =1, P =0
10
0.0
1

TEXTBOX
174
69
357
87
Offspring = Death-rate * NumAgents
10
0.0
1

TEXTBOX
385
416
535
442
Share of Defectors who became Defectors by Mutation
10
0.0
1

TEXTBOX
596
416
759
442
Share of Cooperators who became Cooperators by Mutation
10
0.0
1

TEXTBOX
820
425
1008
443
Average Friendliness ρ of Cooperators
10
0.0
1

TEXTBOX
1034
424
1214
442
Average Friendliness ρ of Defectors
10
0.0
1

TEXTBOX
384
245
534
323
Rationality of Agents\nfor K ≈ 0 Fermi-Function turns into Step function.\nfor K >> 0 agents behave lesser and lesser according to the Best Response Rule
10
0.0
1

TEXTBOX
1244
195
1601
213
Average Fitness = Av. Pay-Off = Av. Reproduction Probability of Agents
10
0.0
1

TEXTBOX
820
187
970
213
Average Friendliness ρ of Cooperators and Defectors
10
0.0
1

TEXTBOX
1251
424
1411
442
Average Fitness of Cooperators
10
0.0
1

TEXTBOX
1460
425
1610
443
Average Fitness of Defectors
10
0.0
1

@#$#@#$#@
## WHAT IS IT?

A random spatial coincidence of friendly agents can lead to the sudden spreading of other-regarding preferences and a transition from a ‘homo economics’ to a ‘homo socialis’. The graphs show representative simulation runs on a 30 x 30 spatial grid with periodic boundary conditions. 60% of all sites are occupied with agents who can either cooperate or defect. The payoff of interacting agents is determined as sum of payoffs from prisoner dilemma games with all Moore neighbours. The payoff parameters are: ‘Temptation’ T = 1.1, ‘Reward’ R = 1, ‘Punishment’ P = 0, and ‘Sucker’s Payoff’ S = -1. The strategies (cooperation or defection) are updated simultaneously for all agents, applying the myopic best response rule to the utility function of each agent. It weights the payoffs of neighbours with the friendliness ρi	and the own
payoff with (1− ρi) . Agents die at random with probability β = 0.05. To keep population size
constant, surviving agents produce offsprings proportionally to their payoff in the previous round. Offspring move to the closest empty site (ν =1) and inherit attributes from the parent, here: the friendlinessρi. However, with probabilityμ=0.05, the friendliness of offsprings mutates. With probability 0.8 it is ‘reset’ to a uniformly distributed random value between 0 and the friendliness ρi	of the parent, and with probability 0.2 it takes on a uniformly distributed
value between ρi	and 1.

![Average of friendliness and share of cooperating agents as a function of time (one generation is 1 β periods).](file:AverageFriendliness&ShareCooperators.jpg)

(A) Average of friendliness and share of cooperating agents as a function of time (one generation is 1 β periods).

![Average payoffs of cooperators and
defectors as a function of time.](file:AveragePayoffsCooperators&Defectors.jpg)

(B) Average payoffs of cooperators and
defectors as a function of time. Initially, defectors are more successful than cooperators. However, when the sudden transition from a ‘homo economics’ to a ‘homo socialis’ occurs, the payoffs for defectors increases, but the payoffs for cooperators increases even more, which implies higher production rates of agents with other-regarding preferences.
## HOW IT WORKS


Parameters used in the Paper "Both Selfish and Other-Regarding Preferences Result from Natural Selection" by Thomas Grund and Dirk Helbing
––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
– L x L = 30 x 30 with periodic boundary conditions
– 60% of the lattice patches are occupied
– Pay-Offs from Prisoners Dilemma: T = 1.1 > R = 1.0 > P = 0 > S = -1
– Strategyupdate happens simultaneously for all agents according to myopic best-esponse rule to the utility function of each agent (-> K ≈ 0)
– Utility of Agents: U = (1 - ρ) ⋄ (Own Payoff) + ρ ⋄ (all Neighbours Payoff)
   with  ρ = friendliness
– Agents die with probability β = 0.005 %
– Offspring produced at rate proportional to agent's personal pay-off (not utility!) of the last round
– Offspring born next to parents with probability ν ("local reproduction")
– Offspring born further away from parents on a randomly selected free patch with probability (1 - ν) ("random reproduction")
– For Figure 1: ν = 1 -> "full local reproduction"
– friendliness ρ_parent of parent is inherited to offspring, but with mutation probability μ, ρ_child is set to
   ⋄ 0 ≤ ρ_child ≤ ρ_parent with Probability P = 0.8
   ⋄ ρ_parent ≤ ρ_child ≤ 1 with Probability P = 0.2
(if other-error 80% is selected in interface)


## HOW TO USE IT

prob-behavior-change 	Probability that behavior of choosing a strategy is irrational
prob-reproduction 	~ = exp(fitness/L) - "Fit agents reproduce themselves more."
payoffC 		U(C) - utility if strategy is C
payoffD 		U(D) - utility if strategy is D
-> payoff-current 	OBSOLETE - deleted in in Vers 1.0
-> payoff-alternative 	OBSOLETE - deleted in in Vers 1.0
strategy 		0,1 = D,C - Defect, Cooperate 
fitness 		U(C,D) - utility
-> reproduction 	OBSOLETE - deleted in in Vers 1.0
otherregarding 		rho - friendliness
c-neighbors 		c - number of cooperators in neighborhood
d-neighbors 		d - number of defectors in neighborhood
-> needs-updating 	FALSE (not further used in code) - deleted in in Vers 1.0
mutated			TRUE,FALSE - Only relevant for counting mutated agents.

GLOBAL VARIABLES:

hist-other 		???
-> coop-avg 		=(coop-sum/(wait-measuring-until)) TIME AVERAGE -deleted in Vers5
-> other-avg 		=(other-sum/(wait-measuring-until)) TIME AVERAGE -deleted in V.5
-> coop-sum 		Sum for time average calculation  - deleted in in Vers 5.0
-> other-sum 		Sum for time average calculation  - deleted in in Vers 5.0
-> C-above-50 		COMMENTED OUT - deleted in in Vers 2.0
-> other-above-25 	COMMENTED OUT - deleted in in Vers 2.0
P 			Punishment in PD
R 			Reward in PD
coop-share 		Percentage of cooperators
other-all 		Mean rho of all agents
mutated-offsprings 	Number of ~
offspring 		Count of ~
cooperators 		Count of ~
defectors 		Count of ~
num-cooperators 	Number of ~
num-defectors 		Number of ~
other-cooperators 	Mean-rho of all cooperators
other-defectors 	Mean-rho of all defectors
fitness-cooperators 	Mean fitness of all cooperators
fitness-defectors 	Mean fitness of all defectors
mutated-coops 		Number of ~
mutated-defs		Number of ~


## THINGS TO NOTICE



## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="exp50" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-250</go>
    <timeLimit steps="40"/>
    <metric>count turtles</metric>
    <metric>num-cooperators</metric>
    <metric>num-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-250</go>
    <timeLimit steps="40"/>
    <metric>count turtles</metric>
    <metric>num-cooperators</metric>
    <metric>num-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="version1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-500</go>
    <timeLimit steps="20"/>
    <metric>count turtles</metric>
    <metric>num-cooperators</metric>
    <metric>num-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
      <value value="0.75"/>
      <value value="0.8"/>
      <value value="0.85"/>
      <value value="0.9"/>
      <value value="0.95"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="March_6_2012A" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
      <value value="1.2"/>
      <value value="1.3"/>
      <value value="1.4"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
      <value value="0.5"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="March_6_2012B" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
ask agents [ set otherregarding 1]
</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
      <value value="1.2"/>
      <value value="1.3"/>
      <value value="1.4"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
      <value value="0.5"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="March_6_2012C" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="T">
      <value value="0.9"/>
      <value value="0.7"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
      <value value="0.5"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="March_7_2012A" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.1"/>
      <value value="1.2"/>
      <value value="1.3"/>
      <value value="1.4"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu100" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu95" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu90" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu85" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu80" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu75" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu70" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu65" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu60" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu55" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu50" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu45" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu40" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu35" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu30" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu25" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu20" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu15" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu10" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu5" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="z_mu0" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.025"/>
      <value value="1.05"/>
      <value value="1.075"/>
      <value value="1.1"/>
      <value value="1.125"/>
      <value value="1.15"/>
      <value value="1.175"/>
      <value value="1.2"/>
      <value value="1.225"/>
      <value value="1.25"/>
      <value value="1.275"/>
      <value value="1.3"/>
      <value value="1.325"/>
      <value value="1.35"/>
      <value value="1.375"/>
      <value value="1.4"/>
      <value value="1.425"/>
      <value value="1.45"/>
      <value value="1.475"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;exp(payoff/L)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="30april2012" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>count turtles</metric>
    <metric>other-all</metric>
    <metric>coop-share</metric>
    <metric>num-cooperators</metric>
    <metric>num-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
      <value value="1.2"/>
      <value value="1.3"/>
      <value value="1.4"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap0" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap6" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap7" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap8" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap9" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="heatmap10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>calculate-avg-scores</final>
    <timeLimit steps="10000"/>
    <metric>coop-sum / (ticks - wait-measuring-until)</metric>
    <metric>other-sum / (ticks - wait-measuring-until)</metric>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fitness-rule">
      <value value="&quot;(payoff+8*abs(S))&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1"/>
      <value value="1.05"/>
      <value value="1.1"/>
      <value value="1.15"/>
      <value value="1.2"/>
      <value value="125"/>
      <value value="1.3"/>
      <value value="1.35"/>
      <value value="1.4"/>
      <value value="1.45"/>
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wait-measuring-until">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="L">
      <value value="2.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="distribution_rho" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-100</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <metric>num-cooperators</metric>
    <metric>num-defectors</metric>
    <metric>other-cooperators</metric>
    <metric>other-defectors</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <metric>array:to-list hist-other</metric>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fig1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>coop-share</metric>
    <metric>other-all</metric>
    <metric>fitness-cooperators</metric>
    <metric>fitness-defectors</metric>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_coop-share" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>precision coop-share 3</metric>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_other-all" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>precision other-all 3</metric>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fitness-cooperators" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>precision fitness-cooperators 3</metric>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fitness-defectors" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>precision fitness-defectors 3</metric>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_3D-Graph-Stepsize0.02" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>precision  (count  agents  with  [  otherregarding  =  0  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.02  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.04  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.06  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.08  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.1  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.12  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.14  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.16  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.18  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.2  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.22  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.24  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.26  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.28  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.3  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.32  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.34  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.36  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.38  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.4  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.42  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.44  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.46  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.48  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.5  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.52  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.54  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.56  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.58  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.6  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.62  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.64  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.66  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.68  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.7  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.72  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.74  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.76  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.78  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.8  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.82  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.84  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.86  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.88  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.9  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.92  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.94  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.96  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  0.98  ])  3</metric>
    <metric>precision  (count  agents  with  [  otherregarding  =  1  ])  3</metric>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_3D-Graph-Stepsize0.01" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>count  agents  with  [  otherregarding  =  0  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.01  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.02  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.03  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.04  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.05  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.06  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.07  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.08  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.09  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.10  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.11  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.12  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.13  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.14  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.15  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.16  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.17  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.18  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.19  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.20  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.21  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.22  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.23  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.24  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.25  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.26  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.27  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.28  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.29  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.30  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.31  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.32  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.33  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.34  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.35  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.36  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.37  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.38  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.39  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.40  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.41  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.42  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.43  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.44  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.45  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.46  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.47  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.48  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.49  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.50  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.51  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.52  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.53  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.54  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.55  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.56  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.57  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.58  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.59  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.60  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.61  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.62  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.63  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.64  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.65  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.66  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.67  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.68  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.69  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.70  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.71  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.72  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.73  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.74  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.75  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.76  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.77  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.78  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.79  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.80  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.81  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.82  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.83  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.84  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.85  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.86  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.87  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.88  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.89  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.90  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.91  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.92  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.93  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.94  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.95  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.96  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.97  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.98  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  0.99  ]</metric>
    <metric>count  agents  with  [  otherregarding  =  1  ]</metric>
    <enumeratedValueSet variable="K">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="T">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherregarding-mutation">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-error">
      <value value="&quot;80%-down-reset&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="localreproduction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
