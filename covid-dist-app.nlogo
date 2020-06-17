extensions [table]
;extensions [web fuzzy arduino]
;;application simulating covid like desease along a detection pattern that is inverse proportional to the severity factor of the desease
;;people that are no of little symptom are less detectable than people havinf severe ones.

;show n-of 1 turtles  with [health-state < 0]
;set temp turtles with [color = red]
;ask (turtles with [color = red]) [inspect]

;ask turtles [set temp (fput self temp)]

;ask  n-of 1 turtles  with [health-state = 2] [inspect self]


;my-agent-set with [ color = red ]
;filter [ a -> [ color = red ] of a ] my-agent-list

;TODO model3
; detection speed-desease virality   - DETECTION = stop propagation modulated by belief and civility-factor
; FIRST: natural selection   later SECOND (belief)   THIRD (+civility)

; modulate infection



globals
  [
    ;UI people-nb
    ;UI people-nb
end-simulation-time
    day-duration
    lifespan             ;; the lifespan of a turtle

    %infected            ;; what % of the population is infectious
    %immune              ;; what % of the population is immune
    %dead
    %dead-inc
    ;%infected
    %infected1
    %infected2
    %infected3
    %app-user            ;; app mode 1
    %app-contrib         ;; app mode 2 (potential contributor) - difficult to know in advance as can be chosen later - a 1 mode can become 2 whenever he wants
    %app-contrib-per-user

    %detected            ;; number od turtles that have been detected (predetected + test confirmed)

    patient-zero

    ;UI desease-duration-min
    ;UI desease-duration-max

    ;UI proba-infection  ;proba of infection globale 20%
    ;UI proba-symptom ;proba of having symptoms | infection 50%
    ;UI proba-severe; proba of having severe symptoms | symptoms 10%
    proba-death; proba of dying | severe    5%

    app-detection-factor
    ;UI app-pull-users   ;; pourcentage of turtles having the app in pull mode only - we consider that if a case is severe and detected, then the user is considered pull
    ;UI app-push-users   ;; pourcentage of turtles having the app in pull/push mode (potential contributors)

  ;UI simulation-app   ;;0 without app / 1 with app
  ;UI debug-mode  ;to show information in the console
  interactive

    ;chance-detection-assympt;delta-detection-assympt;delta-detection-sympt ;chance-detection-sympt-sev ;delta-detection-sympt-sev
    ;weibull alpha beta
    tirage-number  ;just to check how many random has been called

    desease-speed-min  ;en deca pas de symptome, au dela, repartition degressive - fonction linéaire negative  (exponentielle) x!

    temp   ;a global to store temporary values from the console | set temp value


    ;variable to calculate indicators (store min, max, mean value) so as to calculate infection indicators
    max-has-infected-nb
    min-has-infected-nb

    ;indicator should be at least 2 var : chance of being infected, chance of havins severe health degradation.
    ; we focus on the chance of being infected. We know the global infection probability (around 20%) => make variable (10 to 30% for instances)
    ;see turtle internals
    proba-detectability-k

    alpha-k-limits

    sample-1
    sample-2
    sample-3
    sample-4
    sample-5

]

turtles-own
  [
    age
    positions
    health-state    ;; it represent the severity obtained at maximum (meaning it can be modulated later - combining to other factors for instance)
    ;; 0 healthy, 1 asymptomatic, 2 symptomatic, 3 symptomatic severe; 4 detected (sick but inactive); -1 immune; -2 dead
    health-k
    current-health-val   ;to store the calulation of the state degradation

    app-mode              ;; 0 no app, 1 app pull only (user), 2 app (pull and push) potential contributor, 3 (effective contributor)

    self-resistance      ;; to alter probability to get sick (later)
    desease-duration
    ;infection omniscient memory
    ;has-met ;list   ! Memory   (last desease-duration) excet for healthy / or undetected !
    ;has-met-ticks  ;list
    has-met-dict
    infection-by
    infection-ticks-nb
    ;has-infected ;list
    ;has-infected-ticks  ;list
    has-infected-dict  ;if ticks->0 - it has met and try to infect but none were infected


    infection-per-day   ;so as to have an idea of the R0

    infection-indicator  ;an indicator if infected or not
    ;infection-real-exposure ;an array just to sum infected turtles
    ;infection-estimated-exposure

    detection-time      ;; when the turtle has been detected positive (in ticks)

    ;trapezoidal viral charge and infecting factor (hypothesis)
    infection-start-time  ;moment de l'infection ;; when the turtle has been infected (in days or ticks ?)
    infection-max-start-time
    infection-max-end-time
    infection-end-time  ;no more possible contamination


    natural-detection-start-time

    ;par defaut, si symptomatique (fonction lineaire de t de coefficient directeur severité).
    desease-speed  ;en deca pas de symptome, au dela, repartition degressive - fonction linéaire negative - to get to final health-state (can be altered if medication)
    alpha-max
    alpha-min
    alpha

   ]


;; The setup is divided into four procedures
to setup
  clear-all
  setup-constants
  setup-turtles
  update-global-variables
  update-display
  reset-ticks
  tick ;to start initial infection at 1 (so 0 means no infection)
  initial-infection


end

;; This sets up basic constants of the model.
to setup-constants
  set end-simulation-time 0
  set debug-mode false
  set interactive true
  ;set lifespan 50 * 52 * 7 * 10
  ;set people-nb 1000
  set day-duration 5
  set desease-duration-min 20 ;in days
  set desease-duration-max 20 ;in days

  set proba-infection 20  ;proba of infection globale 20%     //CHECK normal law & weibull
  ;set proba-symptom 50    ;proba of having symptoms | infection 50%
  set proba-severe 10     ;proba of having severe symptoms | symptoms 10%
  set proba-death 50      ;proba of dying | severe    5%

  ;set app-pull-users 50   ;; pourcentage of turtles having the app in pull mode only - we consider that if a case is severe and detected, then the user is considered pull
  ;set app-push-users 10    ;; pourcentage of pull users that are volontar (contribute)

  ;set alpha 6.4  ;en fait mean and standard deviation  set beta 2.1
  set tirage-number 0

  set proba-detectability-k [0 20 40 60 80 100]   ; 100 / k-size * k

  ;set alpha-k-limits (list (4 * 1 / 3) (4 * 2 / 3) (4 * 3 / 3) (4 * 4 / 3) (4 * 5 / 3) (4 * 6 / 3))
  set sample-1 0
  set sample-2 0
  set sample-3 0
  set sample-4 0
  set sample-5 0

end

to setup-turtles
  create-turtles people-nb
    [ setxy random-xcor random-ycor
      set age random lifespan  ;; for later variation ?  infectiosity / age  80yo+
      set size 1  ;; 1 patch
      set shape "circle"

      set positions []

      set health-state 0
      set health-k 0
      set current-health-val 0

      set app-mode set-app-mode

      set self-resistance  1    ;; to alter probability to get sick (later) - probabilty with  age param
      ;set desease-duration 0

      ;infection memory/history (omniscient)
      set infection-start-time 0
      ;set has-met [] ;list   ! Memory   (last desease-duration)
      ;set has-met-ticks []  ;list
      set has-met-dict table:make  ;a dictionary to keep ticks->value
      ;turtle memory for infection chain / true & potentials | WARNING memory
      set infection-by 0
      set infection-ticks-nb 0
      ;set has-infected []        ;list of the last 2/3 desease duration
      ;set has-infected-ticks []  ;correponding tick list
      set has-infected-dict table:make
      ;set infection-per-day   ;so as to have an idea of the R0 => can be calculated

      set desease-duration (desease-duration-min + random-float (desease-duration-max - desease-duration-min))

      ;;trapezoidal modeling of the viral charge that lasts 3/4 of the desease duration.  //can be calculated...
      set natural-detection-start-time desease-duration / 4      ;; when the turtle has been detected positive (in days or ticks ?)
      set infection-max-start-time desease-duration / 4
      set infection-max-end-time desease-duration / 2
      set infection-end-time desease-duration * 3 / 4  ;no more possible contamination

      set detection-time 0      ;; when the turtle has been detected positive (in days or ticks ?)


    ;par defaut, si symptomatique (fonction lineaire de t de coefficient directeur severité).
      set desease-speed-min 0 ;en deca pas de symptome, au dela, repartition degressive - fonction linéaire negative
      set alpha-max 0
      set alpha-min 0

;TODO ;variable to calculate indicators (store min, max, mean value) so as to calculate infection indicators
    ;global but local max-has-infected-nb
    ;global but local too min-has-infected-nb
    ;refactor V4 ou V5 => encapsulate ticks and value in the same struct, has min, max, mean...


      set infection-per-day 0   ;so as to have an idea of the R0   => = mean of... contaminated people for one people
      set infection-indicator [] ;an indicator if infected or not
      ;set infection-real-exposure [] ;an array just to sum infected turtles
      ;set infection-estimated-exposure []

  ]

  ;inspect one-of turtles
end


to setup1
  setup
  set debug-mode false
  ;set lifespan 50 * 52 * 7 * 10
  set people-nb 1500

  set desease-duration-min 12 ;in days
  set desease-duration-max 18 ;in days

  set proba-infection 20  ;proba of infection globale 20%
  set proba-symptom 90    ;proba of having symptoms | infection 50%
  set proba-severe 10     ;proba of having severe symptoms | symptoms 10%
  set proba-death 50      ;proba of dying | severe    5%


  set app-pull-users 50   ;; pourcentage of turtles having the app in pull mode only - we consider that if a case is severe and detected, then the user is considered pull
  set app-push-users 10    ;; pourcentage of pull users that are volontar (contribute)

end

to setup2
  setup
  set debug-mode false
  ;set lifespan 50 * 52 * 7 * 10
  set people-nb 1500

  set desease-duration-min 12 ;in days
  set desease-duration-max 18 ;in days

  set proba-infection 10  ;proba of infection globale 20%
  set proba-symptom 10    ;proba of having symptoms | infection 50%
  set proba-severe 10     ;proba of having severe symptoms | symptoms 10%
  set proba-death 50      ;proba of dying | severe    5%


  set app-pull-users 50   ;; pourcentage of turtles having the app in pull mode only - we consider that if a case is severe and detected, then the user is considered pull
  set app-push-users 10    ;; pourcentage of pull users that are volontar (contribute)

  ;set alpha 6.4  ;en fait mean and standard deviation  set beta 2.1
  set tirage-number 0
end

to update-global-variables
  if count turtles > 0
    [ ;;set %infected (count turtles with [ sick? ] / count turtles) * 100
      set %immune (count turtles with [ health-state = -1 ] / count turtles) * 100
      set %dead (count turtles with [ health-state = -2 ] / count turtles) * 100
      set %dead-inc (count turtles with [ health-state = -2 and (ticks - infection-end-time <= 5 * day-duration)  ] )
      set %infected (count turtles with [ health-state > 0 ] / count turtles) * 100
      set %infected1 (count turtles with [ health-state = 1 ] / count turtles) * 100
      set %infected2 (count turtles with [ health-state = 2 ] / count turtles) * 100
      set %infected3 (count turtles with [ health-state = 3 ] / count turtles) * 100

      set %app-user (count turtles with [ app-mode > 0 ] / count turtles) * 100
      set %app-contrib (count turtles with [ app-mode = 2 ] / count turtles) * 100
      set %app-contrib-per-user (count turtles with [ app-mode = 2 ] / count turtles with [ app-mode = 2 ]) * 100
      set %detected (count turtles with [ detection-time > 0 ])]
end

to-report set-app-mode
  ifelse simulation-app = true
    [
      ;simulation prenant en compte applications
      ; USER (pull) / PUBLISHER (push-pull) %app-users %app-contributors
      ifelse random-float 100 < app-pull-users
        [
          ifelse random-float 100 < app-push-users
             [report  2]
             [report 1]
        ]
        [report  0]
    ]
    [report  0]  ;; analyse sans prise en compte des applications
end

to update-display
  ask turtles
    [
      let current-h current-health-state
      ;if shape != turtle-shape [ set shape turtle-shape ]
      (ifelse
        current-h = -1 [
          set color green
          ;set plabel "immune"
        ]
        current-h = -2 [
          set color brown
          ;set plabel "dead"
        ]
        current-h = 0 [  ;current-state-cat
          set color gray
          ;set plabel "healthy"
        ]
        current-h = 1 [
          set color yellow
          ;set plabel "asymp"
        ]
        current-h = 2 [
          set color [255 165 0]
          ;set plabel "sympt"
        ]
        current-h = 3 [
          set color [255 140 0]
          ;set plabel "severe"
        ]
        current-h = 4 [
          set color [255 69 0]
          ;set plabel "severe"
        ]
        current-h = 5 [
          set color [255 0 0]
          ;set plabel "severe"
        ]

        ; elsecommands
        ;[
          ;set pcolor green
          ;set plabel "0"
      ;]
      )
  ]
end

to-report current-health  ;function that returns the variable state (health-state being the final target) => more health-k ?
    ifelse health-state > 0
      [
        let val 0
        let ticks-since-infection ticks - infection-start-time
        ifelse ticks-since-infection <  (desease-duration-ticks / 8)
          [set val 0] ;MIN
          [ifelse ticks-since-infection < (desease-duration-ticks / 4)
            [set val alpha * (ticks-since-infection - (desease-duration-ticks / 8))]  ;linear growth of the virality (affect random-infect)
            [
              if ticks-since-infection < (desease-duration-ticks)
                [
              ;set val virality-max
              set val  alpha * (desease-duration-ticks / 8) ]  ;max virality
                ;[
                ;  ifelse ticks-since-infection < (desease-duration-ticks * 3 / 4)
                ;     [set val virality-max - (alpha * (ticks-since-infection -  (desease-duration-ticks / 2) - (desease-duration-ticks / 8) ))] ;inverse growth
                ;     [set val 0] ;MIN0
                ;]
            ]
          ]
         set current-health-val val
         report val
       ]
       [report health-state]
              ;d/8 <= x < d/4
              ;   X = x - d/8
              ;   f(X) = alpha * X      [ current-health-val ] of n-of 1 turtles  with [health-state = -1]
end

to-report health-max-degradation
  ifelse health-state = 0
    [report 0]
    [ ifelse detection-time = 0
        [report precision (alpha * (3 * desease-duration-ticks / 4 -  ( desease-duration-ticks / 8))) 3]
        [report precision (alpha * (detection-time - infection-start-time - (desease-duration-ticks / 8))) 3] ]
end

to-report virality-max
  report precision (alpha * (desease-duration-ticks / 8)) 2 ;- (desease-duration-ticks / 8))
end

to-report current-health-state
  ;6 categories
  ifelse health-state < 1
    [report health-state]
    [report min list health-k round (current-health / 10)]   ;empirique
end

to initial-infection ;; turtle procedure
  ask n-of 1 turtles [  ;;mettre un parametre ?
    if debug-mode [inspect self]
    set patient-zero self
    set infection-by self
    set infection-ticks-nb ticks ;should be 1
    set infection-start-time ticks
    set health-state 1  ;asymptomatic / k=0 as no symptom
  ]
end

to-report random-infect   ;infect according to probability - set samples for drawing - set alpha1 and alpha2 - set infection variables (DEBUG)
  ;TODO - check if correct - use variable for proba - modulate with virality linear 1/4 => max 1/2 => -linear 3/4 > 0 end
  if random-float 100 < proba-infection
     [
         ifelse random-float 100 > proba-symptom
            [set health-state 1
            ;print "error"
            ] ; health-k=0   asymptomatic       healt-k-limits [ 40  70  90 ]
            [
              let random-k random-float 100
              ifelse random-float 100 < proba-severe
                  [
                    set health-state 3
                    set health-k 5
                    if sample-5 = 0 [ set sample-5 self]
                  ]
                  [
                    set health-state 2
                    ifelse random-k < 40
                      [set health-k 1
                      if sample-1 = 0 [set sample-1 self]]   ;use a global parameter / eventually a modulo + [40 30 20 10]
                      [ifelse random-k < 70

                          [set health-k 2
                          if sample-2 = 0 [set sample-2 self]]
                          [
                            ifelse random-k < 90
                                [set health-k 3
                                if sample-3 = 0 [set sample-3 self]]
                                [set health-k 4
                                if sample-4 = 0 [set sample-4  self]];insert-item 4 samples self
                          ]
                      ]   ;choose k amongs 5 categorie, last one beeing for 3 state

                  ]
             ]
        ;infect at diverse degree, a state and a severity that is proportional
        ;y1(k) = 4k/15.duree.x - k/30
        ;y2(k) = 4k/3.duree.x - k/6

        ;proba-detectability-k [0 20 40 60 80 100]  too sharp or every 2 days ? for the last day. [0 10 20 30 40 50] first day then [0 20 40 60 80 100] bof  [0 5 20 30 40 50]
        ; I use the line to model the varaiation but if in area 1 it has not to be detected early - fine tune this parameter is touchy (to explore)
        ;                      [K0  K1 K2 K3 K4 ...
        set alpha-max  4 * health-k / 3
        ;set alpha-min  4 * health-k / 15
        set alpha-min  4 * health-k / 8

        let dif alpha-max - alpha-min
        set alpha alpha-min + random-float dif

        set infection-by myself
        set infection-start-time ticks
        report self
     ]
  report 0
end

to go  ;;step method - main loop   => manque detection +  annonce passage push   +   moyen d'enregistrer activer...  tous les voisins ?
  ;stop simlation condition - no more infected turles
  if (all? turtles [health-state < 1]) [
    set end-simulation-time ticks ;restart if < 200 ?
    end-of-simulation-report
    stop]

;  if temp != 0      ;DEBUG
;    [inspect temp
;    stop]

  ask turtles [ ;move and end desease when necessary
    recover-or-die-when-desease-over ;final state for infected turtles after the full duration of the desease
    move
  ]

  ask turtles [

    log-met-people
    ;:if infected and viral?
    if health-state > 0 [
      if detection-time = 0   ;real sick not yet detected so can infect
        [infect-collect-neighbor] ;no else condition as if sick AND detected, then inactive => self-confinment / hospital

    ;each day - detect naturally and update infection indicators
    if (ticks + infection-ticks-nb + 1)  mod day-duration = 0  ;plusieurs ticks par jours / on verifie tous les jours...
        [
          if detection-time = 0 [
            detection-try-each-day]
          update-infection-indicator ;slowing down if by ticks => ok if by day only - wich is logical anyway, a bit wrong though but negligible ?
        ]
      ]
    ]
    update-global-variables
    update-display
    ;set day ticks / day-duration

    tick  ;increment steps
end

to set-samples
  set sample-1 0
  set sample-2 0
  set sample-3 0
  set sample-4 0
  set sample-5 0
  ;foreach [1 2 3 4 5]
  ;  [ k -> set samples fput n-of 3 turtles  with [health-state = k] samples ]
end

to move ;; turtle procedure  pure random move by 1 - DEBUG position log
  if health-state != -2 [
    rt random 100 ;;The turtle turns right by number degrees, relative to its current orientation. Strange !
    lt random 100  ;;The turtle turns left by number degrees, relative to its current orientation.
    fd 1 ;; avance de 1
    ;;collect position here ?
    if detection-time = 0
      [ ;we do not keep positions once detected as we suppose the patient is system confined. need ticks to be useful
        if length positions >= desease-duration * day-duration  ;refine later as this si calculated by has-met and friends
          [set positions but-last positions]   ;should certainly be longer especialy for O.5 healt-state (lucky turtles exposed but not infected
        set positions fput (list xcor ycor) positions
      ]
  ]
end

to recover-or-die-when-desease-over ;; Once the turtle has been sick long enough, it either recovers (and becomes immune) or it dies.
    ;;final state for infected turtles after the full duration of the desease
    if health-state > 0
       [  if ticks-since-infected > (desease-duration * day-duration)                      ;; end of the desease
        [ ;if debug-mode [print "immune or die"]
          set infection-end-time ticks
          ifelse random-float 100 < proba-death   ;; either immune or die
          [set health-state -1]     ;;become-immune
                                    ;move to corner and chage size to indicate visually the death level | experimentation ^
          [ifelse health-state = 3
            [
              set xcor min-pxcor
              set ycor max-pycor
              set size count turtles with [health-state = -2] / max-pxcor  * 10
              set health-state -2
             ]
             [set health-state -1]]   ;die or immune if no severe health state
        ]
       ]

end

to log-met-people

  let met sort other turtles-here with [health-state > -2 and detection-time = 0]   ;we don't log when meeting a dead turtle (wich is still on the simulation)
  ; ! also -1 that were detected   => we don't need to log them as they don't affect simulation
   if not empty? met [
     ;print (word ticks " turtle " self " met " ([self] of other turtles-here))   ;DEBUG
     table:put has-met-dict ticks met
  ]

end

to infect-collect-neighbor ;; If a turtle is infected, at each step, it can random-infect turtles on the same patch.
   let infected-turtles-by-self []
   ask other turtles-here with [ health-state = 0 ][
      let infected-turtle random-infect
      if infected-turtle != 0
        [
          set infection-by myself
          set infection-ticks-nb ticks  ;remove one of these two variables representing the same information
          set infection-start-time ticks
          set infected-turtles-by-self fput infected-turtle infected-turtles-by-self  ;table:put ?
          ;set temp self
          ]

    ]
   if not empty? infected-turtles-by-self [
     table:put has-infected-dict ticks infected-turtles-by-self
    ]
   ;documulative infection reinforcment (state > 0)
end

to-report virality-infection-factor ;DEBUG modulate infection chances with this factor
     let val 0
     let ticks-since-infection ticks - infection-start-time
        ifelse ticks-since-infection <  (desease-duration-ticks / 4)
          [set val 400 * ticks-since-infection / desease-duration-ticks ] ;incressing
          [ifelse ticks-since-infection < (desease-duration-ticks / 2)
            [set val 100]  ;linear growth of the virality (affect random-infect)
            [
              ifelse ticks-since-infection < (desease-duration-ticks * 3 / 4)
                [set val 100 - (400 * (ticks-since-infection - (desease-duration-ticks / 2)) / desease-duration-ticks) ]  ;decressing virality
                [set val 0]
            ]
          ]
         report precision val 2
end


to detection-try-each-day ; depends on healt-k relative probabily  k=5 detectability-chance = 100 DEBUG (long detection time around 22 days in echa categories
    ;detection de la maladie (natural vs. asisted by tests and/or app)      ;detection-time = 0 (precondition on the call)
    ;detection is a probability function depeding on the dynamic health status - health-k related
     ;natural detection depends on symptoms - the more the obvious is detectability.
               ; DEBUG depends on current-health  k=5 detectability-chance = 95 (or it must happen in 2 days max - 95% day means only 1/400 chance of not detecting in two days
           let proba-sympto-detectability 0

  ;health = current-health-state

           if  current-health-state = 5 [print "--> det 5 "
                        set proba-sympto-detectability 90]  ;BUG  ok surely because I stop desease progression once detected...
                                                                              ;FALSE as health-state is a potential ? CHECK later
           if  current-health-state = 1 [;print (word "--> det 1 " health-k " current: " current-health-state)
                                         set proba-sympto-detectability 0]   ;0 5 10 30    100
           if  current-health-state = 2 [;print (word "--> det 2 " health-k " current: " current-health-state)
               set proba-sympto-detectability 5]
           if  current-health-state = 3 [;print (word "--> det 3 " health-k " current: " current-health-state)
               set proba-sympto-detectability 10]
           if  current-health-state = 4 [;print (word "--> det 4 " health-k " current: " current-health-state)
               set proba-sympto-detectability 30]

     ;ICI detection
           ifelse random-float 100  <  proba-sympto-detectability
           [
               print (word "DET --> det " current-health-state " hk: " health-k " time-since-inf " print-time  (ticks - infection-start-time)  )
               set detection-time ticks
               set size 2
           ]  ;then detect/become inactive
           [ ; FORCE all turtle to be detected - unrealistic but interesting simulation parameter - maybe put a var to control the output - 100% right now
             if ticks - infection-start-time > 3 / 4 * desease-duration * day-duration   ;we ensure that at the end of desease, the sick is detected / not real but for simulation purposes reason
             [set detection-time ticks
               set size 4]
           ]

end

to update-infection-indicator
   ;;on ecah day after detection try - update indicator ie. person met that are pushing and detected contaminated.
   ;;note that a met person will be detected later in time, hence the signal comes at leat a day after. When you met a person, you don't know she's infected as she's not yet detected.
   ;=> do we collect indication for each days ? can be around 300 values.
  ; [ [1] [2] [3] [detected]]

;  - a1 (ra1) - nb of user that were met and declared (detected) infected n days after we met.
;  - A2 -    ---------- detected on the whole simulation time(modifié)
;
;  - B1 - nb of user that were met but not detected (possibily three states - healthy, infected, immune) n days before now
;  - B2 - ---------- during the whole simulation time
;
;  - C cumulative pushers (once detected)

  ; indicator-full [a-n a-all b-n b-all ra-n ra-all rb-n rb-all c]
  ; indicator [ squareroot (a-n * a-all) squareroot (ra-n * ra-all) ]
  ; reality [0 0 0 0 0 0 1 1 1 2 2 2 3 3 3 X]
  ; reality [0 0 0 0 0 0 1 1 1 2 2 2 3 3 3 2 2 2 1 1 1 0 0 0]

  ; [met-all-push-detected]
   if detection-time = 0   ;WARNING - there is a late effect as people during the last days met can me detected.
     [ ;ticks - 5 < detection-time
       let indicator initialize-infection-indicator
       compute-indicator indicator
       add-fifo-memory indicator
       ;fput
       ;compute-indicator indicator
     ]
end

;;trop trop lent  sature l'ordi...   repenser...
to-report initialize-infection-indicator
    let indic table:make
    table:put indic "turtle" self
    table:put indic "ticks" ticks
    table:put indic "infected" infection-start-time
    table:put indic "sick" sick?
    table:put indic "detected" detected?
    ;locat t information (should be a subtable) to ease recursion
    ;table:put indic "tick" [let local-dict  table:make (word ticks)]
    ;table:put indic "met-period-groups" []
    ;table:put indic "met-period-push-groups" []
      ;table:put indic "met" 0
    ;table:put indic "met-infected" 0
    ;table:put indic "met-push" 0
    ;table:put indic "met-infected-push" 0

    ;then the calculated
      ;table:put indic "detected-real" 0
      ;table:put indic "infected-current" 0
      ;table:put indic "infected-all" 0
      ;table:put indic "met-push" 0
      ;table:put indic "detected-push" 0
    ;table:put indic "cumulative-met-infected" 0
    ;table:put indic "cumulative-met-push" 0
    ;table:put indic "cumulative-met-infected-push" 0
      ;table:put indic "value" 0

    report indic
end

to add-fifo-memory [indicator]
    ;let t-min ticks - (10 * day-duration)
    set infection-indicator fput indicator infection-indicator
    if length infection-indicator > 15 ;10 => rollback-days-duration
      [set infection-indicator but-last infection-indicator]
end

to compute-indicator [indicator]
   ;select on each of the last 10 days indicator
   let t-max ticks
   let t-min ticks - (10 * day-duration)
   let met   has-met-since t-min
  ;print met
;  table:put indicator "met" length met
;
;
;  let t filter [p -> [app-mode] of p = 2] met
;  table:put indicator "met-push"  length t
;
;  set t filter [p -> [app-mode] of p = 1] met
;  table:put indicator "met-pull"  length t
;
;  set t filter [p -> member? [app-mode] of p [1 2]] met
;  table:put indicator "met-users (push-pull)"  length t
;
;  set t filter [p -> [app-mode] of p = 0] met
;  table:put indicator "met-no-app"  length t
;
;
;
;  set t filter [p -> [health-state] of p > 0] met
;  table:put indicator "infected-current"  length t
;
;  set t filter [p -> [infection-start-time] of p > 0] met
;  table:put indicator "infected-all"  length t
;
;  set t filter [p -> [detection-time] of p > 0 ] met
;  table:put indicator "detected-real"  length t
;
;  set t filter [p -> [app-mode] of p = 2] met
;  table:put indicator "met-push"  length t
;
;  set t filter [p -> member? [app-mode] of p [1 2]] met
;  table:put indicator "met-users (push-pull)"  length t
;
;  set t filter [p -> [detection-time] of p > 0] t
;  table:put indicator "detected-push"  length t
;
;
;  if length t > 0 [print indicator]
;  ;if length met not empty? [table:put indicator "infected-real" length met with [health-state > 0]]
;
;  table:put indicator "value" length t
;  ;;comment out all the reste to see speed


end

to xx [number]
    ask turtle number [
      print-current-indicator]
end

to xxx
    ask turtle (random people-nb) [
      print-current-indicator]
end

to xxxx
    ask turtle random people-nb [
      inspect self
      print-current-indicator]
end

to print-current-indicator  ;to debug  / only last 15 days prior to detection ?
                            ;but what for people that got immune and undetected... we keep all life ?
                            ;we can imagine that tests confirm positivity or not - useful for actual
  let t-min ticks - (20 * day-duration)
  let met   has-met-since t-min

  print (word "DEBUT === met-ALL-since 15d " length met met)
  print (word self " was infected-by " infection-by " at " infection-start-time " detected time: " detection-time " contagious: " contagious)

  let t filter [p -> [app-mode] of p = 2] met
  print (word "met-push " length t t)

  set t filter [p -> [app-mode] of p = 1] met
  print (word "met-pull " length t t)

  set t filter [p -> member? [app-mode] of p [1 2]] met   ;users (all having app => those concerned by the estimation)
  print (word "met-users (push-pull) " length t t)

;;TODO here -  rajout et test des choses significatives... variation param
; met / met(infected) / met(detected)    => surtout met detected push  ...> aussi pull car le rapport tend vers la realité

  set t filter [p -> [app-mode] of p = 0] met
  print (word "met-no-app " length t t)



  set t filter [p -> [health-state] of p > 0] met
  print (word "infected-current " length t t)

  set t filter [p -> [infection-start-time] of p > 0] has-met-since 0
  print (word "infected-all " length t t)

  set t filter [p -> [infection-start-time] of p > 0 and [detection-time] of p > 0] has-met-since 0
  print (word "infected-all-detected " length t t)

  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1 2] ] has-met-since 0
  print (word "infected-users " length t t)

  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1 2] and [detection-time] of p > 0 ] has-met-since 0
  print (word "infected-users-detected " length t t)

  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [2] and [detection-time] of p > 0 ] has-met-since 0
  print (word "infected-users-detected-push " length t t)

  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1] and [detection-time] of p > 0 ] has-met-since 0
  print (word "infected-users-detected-pull " length t t)

    set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1] and [detection-time] of p = 0 ] has-met-since 0
  print (word "infected-users-undetected-pull " length t t)

  ;set t table:counts has-met-dict
  ;table:group-agents has-met-since 0 [ dtection-time > 0 ]
  ;print (word "infected-all-grouped " length t t)

  set t table:counts sort has-met-since 0   ;   all by frequencies => dict  turtle => met times (lose ticks)
  print (word "infected-all-grouped " table:length t t)

  set t filter [p -> [detection-time] of p > 0 ] met
  print (word "detected-real " length t t)

end

to yyy [number]
    ask turtle number [
      inspect self
      info-indicator]
end

to yy [number]
    ask turtle number [
      info-indicator]
end

to zz [number]
    ask turtle number [
      info-indicator-turtle]
end

to info-indicator-turtle
  let t-min ticks - (20 * day-duration)
  let met-20d has-met-since t-min
  let met-all has-met-since 0
  print ""
  print word "=== START APP INDICATOR INSPECTOR === at " print-time ticks
  print (word "turtle: " self " " current-health-state "/" health-k " cur: " current-health " max: "  health-max-degradation  " vir " virality-max "/" virality-infection-factor)

  ifelse infection-start-time = 0
    [print "  INF: NO"]
    [print (word "  INF: YES " print-time infection-start-time " / " print-time infection-end-time  " by " infection-by " " [detection-time] of infection-by " = has infected => " table:length has-infected-dict " " has-infected-dict)

  let temp-val 0
  ifelse detection-time = 0 [set temp-val "NO"][ set temp-val (word "YES " print-time detection-time)]
  print (word "  DET: " temp-val)
  let app "NO"
  if app-mode != 0 [ifelse app-mode = 1 [set app "YES pull"][set app "YES pull/push"]]
  print (word "  APP: " app )
  print ""
  print "= ANALYSES - MET ratio (only undetected are important)"

  ;simulation related, infected since ... detected since...
  ;print (word " was detected at: " print-time detection-time " ie. " print-time (detection-time - infection-start-time) " after infection")
  ;print (word " was infected " print-time (ticks - infection-start-time) " ago and detected " print-time (ticks - detection-time) " ago")

  ifelse detection-time = 0
        [print "not detected => see app analysis-indications (below)"]
        [
          print (word " infected: " print-time infection-start-time "detected: " print-time detection-time)
          print (word "GUESS (check) : infection date between  " print-time (detection-time - (20 * day-duration)) " and " print-time detection-time)
          print " as detected, we stop analyses for this turtle => later, try to find possible infector in the chain of met people ... V2 V3"
        ]
          ;print (word "GUESS: infection date between  " print-time (detection-time - (3 / 4 * desease-duration-ticks)) " and " print-time detection-time)
        ]   ;" contagious: " contagious

   print " = analyses for undetected - An Aall RAn RAall C"
   ;either infected or not => I partition on detection now + app user / contributors

   ifelse detection-time = 0 [;undetected / worst cases as we don't know if healthy, sick or immune => this is the classic use case / we want the app to predict
      ;the last 15d can be associated to eventual sick state (current)
      ;the the rest can be related to being immune, ie. infected prior the 15d before detection.

      ;we're interested in met person that will be detected later and that are pushing information.
      ;if undetected, we know "nothing"... we could know that such met turtle was not confirmed sick/immune but all output are possible (and is the majority)

      ;detected are sick people being detected, later do detected-immune (for tests)

      ;find met people that were detected less than 15 days after they met me

      let t []
      let detection-backpropagation 20 * day-duration ;desease duration

      ;to undetected, we can say them are you infected or were you infected  VS healthy - if simulation over (its a prediction on having


      ;let real-all has-met-from-to infection-start-time detection-time

      let all-met has-met-from-to 0 ticks

      print (word " :all met: " length all-met " " all-met)

      set t filter [p -> member? [app-mode] of p [1 2] ] all-met
      let all-met-users t
      print (word " :all met users: " length all-met-users " " all-met-users)

      set t filter [p -> [app-mode] of p = 2 ] all-met
      let all-met-contributors t
      print (word " :all met contributors: " length all-met-contributors " " all-met-contributors)


      set t filter [p -> [app-mode] of p = 2 and detection-time > 0] all-met-users    ;in reasonable time frame !  and detection-time is-possible
 ;members? has-met-from-to det'ection-time - detection-backpropagation
 ; OU
 ; un algo direct... sur le disctinaire  en fenetre mobile ?



      let all-met-detected-contributors t
       print (word " :all met sick/detected contributors: " length all-met-detected-contributors " " all-met-detected-contributors)

      print ""
      set t filter [p -> [health-state] of p != 0 and [detection-time - infection-start-time] of p <= detection-backpropagation] all-met
      let all-met-sick-omni t
       print (word " :all met sick (omni): " length all-met-sick-omni " " all-met-sick-omni)

       print ""

      ;set t filter [p -> [app-mode] of p = 2 and [infection-start-time] of p > 0 and [infection-start-time] of p <= detection-time] all-met


     let last-met has-met-from-to (ticks - detection-backpropagation) ticks

      print (word " :last met: " length last-met " " last-met)

      set t filter [p -> member? [app-mode] of p [1 2] ] last-met
      let last-met-users t
      print (word " :last met users: " length last-met-users " " last-met-users)

      set t filter [p -> [app-mode] of p = 2 ] last-met
      let last-met-contributors t
      print (word " :last met contributors: " length last-met-contributors " " last-met-contributors)


      set t filter [p -> [app-mode] of p = 2 and detection-time > 0] last-met-users
      let last-met-detected-contributors t
       print (word " :last met sick/detected contributors: " length last-met-detected-contributors " " last-met-detected-contributors)

      print ""
      set t filter [p -> [health-state != 0] of p] last-met
      let last-met-sick-omni t
       print (word " :last met sick (omni): " length last-met-sick-omni " " last-met-sick-omni)


      print ""
      print (word "POSSIBLE INFECTION: " length last-met-detected-contributors " > real " sick?  )
      print (word "POSSIBLE IMMUNITY: (without detection): " length all-met-detected-contributors " > real " immune?  )

      print ""
      print "note that the number of contributor met is important to consider data relevance... out of 10 people, ...  out of 2 people..."

      print "at end of simulation - POSSIBLE INFECTION MUST BE FALSE."

      set t collect-possible-infecting-turtles
      set last-met t
      print (word " :met potential: " length last-met " " last-met)

      set last-met-users filter [p -> member? [app-mode] of p [1 2] ] t
      print (word " :met potential users: " length last-met-users " " last-met-users)

      set last-met-contributors filter [p -> [app-mode] of p = 2 ] t
      print (word " :met potential contributors: " length last-met-contributors " " last-met-contributors)

 ;HERE 25 mai

      set t collect-all-possible-infecting-turtles
      set last-met t
      print (word " :met potential: " length last-met " " last-met)

      set last-met-users filter [p -> member? [app-mode] of p [1 2] ] t
      print (word " :met potential users: " length last-met-users " " last-met-users)

      set last-met-contributors filter [p -> [app-mode] of p = 2 ] t
      print (word " :met potential contributors: " length last-met-contributors " " last-met-contributors)


      ]

;  print "Known and estimated information (for instance we can't know all person we have met - the simulation can"
;  print ""
;  print "Numbers (real)"
;  print (word length met-all " met turtle points (a turtle can be met several times)"  met-all)
;  let t table:counts sort met-all
;  print (word table:length t " met turtles (grouped)"  t)
;
;  print ""
;  print "Numbers (app inference)"

;;TODO here -  rajout et test des choses significatives... variation param
; met / met(infected) / met(detected)    => surtout met detected push  ...> aussi pull car le rapport tend vers la realité


    [ ;detected
      ;detected / simples cases - but unknown time of infection => estiamation ?   can be today, yesterday, ... up to ? around 15/20 days... TO ADJUST elegantly
      ;useful to try to predict infection time - in the last 3/4 duration
      print (word "as detected, show met turtle for the last 20 days before detection - must contain infector)")
      let t 0
      print ""
      let all has-met-from-to infection-start-time detection-time
      print (word length all " met turtles (ALL) between infection and detection " all)
      set t filter [p -> member? [app-mode] of p [1 2] ] all
      print (word " dont " length t " app users (PULL+PUSH) " t)
      set t filter [p -> [app-mode] of p = 1 ] all
      print (word "   " length t " app simple users (PULL only) " t)
      set t filter [p -> [app-mode] of p = 2 ] all
      print (word " et " length t " app contributors (PUSH only) " t)

   ;ICI
      set t filter [p -> [app-mode] of p = 2 and [detection-time] of p > 0 ] all
      print (word " dont " length t " PERSONNES DETECTEES" t)
      set t filter [p -> [infection-start-time] of p > 0 and [infection-start-time] of p <= detection-time ] all
      print filter [p -> detection-time - [infection-start-time] of p  < desease-duration-ticks ] all
      print (word "   amongst " length t " (real met infected omni)" t)

;TO DEBUG

      set t filter [p -> [infection-start-time] of p > 0 and [infection-start-time] of p <= detection-time ] all
      print filter [p -> detection-time - [detection-time] of p  > 0 ] all

      ;use infection-end-time

      print (word "   amongst " length t " (real met infected undetected)" t)



      print ""
      let all-estim has-met-from-to (detection-time - (20 * day-duration)) detection-time
      print (word length all-estim " met turtles (ALL for 20 days) between (ESTIMATED) infection and detection " all-estim)
      set t filter [p -> member? [app-mode] of p [1 2] ] all-estim
      print (word " dont " length t " app users (PULL+PUSH) " t)
      set t filter [p -> [app-mode] of p = 1 ] all-estim
      print (word "   " length t " app simple users (PULL only) " t)
      set t filter [p -> [app-mode] of p = 2 ] all-estim
      print (word "   " length t " app contributors (PUSH/PULL) " t)

      set t filter [p -> [app-mode] of p = 2 and [detection-time] of p > 0 ] all-estim
      print (word " dont " length t " PERSONNES DETECTEES" t)

      ;TO DO => filter n days before detection
      ;set t filter [p -> [detection-time] of p > 0 ] met
      ;set t filter [p -> member? [app-mode] of p [1 2] ] all-estim

      ;  print (word "infected-users-detected " length t t)
      ;
    ]
end

to-report collect-possible-infecting-turtles      ;-at now    duplicate of -- has-met-all-possible-infector -- whether app or not - filter later thos who have app and are push
  let t ticks - 15 * day-duration   ;desease-duration-ticks            ;for immune only --
  let detection-backpropagation 15 * day-duration

  let result []

  foreach ( table:keys has-met-dict ) [ ; all met-array at key ticks => problem is to select only those that are detected later in time,
                                        ; but according to a certain period (maximum period of infection (20 days first)
    [key] -> if key >= t   ;key is met moment
       [
         let val table:get has-met-dict key
         ;either 0 => at detection-time, hence no report possible  //  should collect up to ? no  will be available later...
         ;=> undetected /
         let temp-val filter [p -> [detection-time] of p > 0 and [detection-time] of p - key < 15] val   ;use 20 as veriable like backpropagation-time
         set result sentence temp-val result                               ;
       ]
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end

to-report collect-possible-infecting-turtles-at [tick-nb]     ;duplicate of -- has-met-all-possible-infector -- whether app or not - filter later thos who have app and are push
  let t tick-nb - 15 * day-duration   ;desease-duration-ticks            ;for immune only --
  let detection-backpropagation 15 * day-duration

  let result []

  foreach ( table:keys has-met-dict ) [ ; all met-array at key ticks => problem is to select only those that are detected later in time,
                                        ; but according to a certain period (maximum period of infection (20 days first)
    [key] ->

      if key <= t
        [
         let met-at-key table:get has-met-dict key
         let temp-val 0
         ;either 0 => at detection-time, hence no report possible  //  should collect up to ? no  will be available later...
         ;=> undetected /

         ;at insertion... key is when meeting - the met turtle p must not have its detection time different than 0 OR must be > key since once detected turtles met are no longer logged


         ;not so simple = main problem is meeting again undetected ?   not really but could be the bug

         foreach met-at-key [p ->
           if [detection-time] of p > 0 ; >= key
           [
 ;debug
             if [detection-time] of p < (15 * day-duration + key) and ([detection-time] of p >= (tick-nb - (15 * day-duration)))  ;explain magic 20 and - 15   ! ouch  it has changed
               [
               print (word p  " key " key " det: " [detection-time] of p " key " key " diff " ([detection-time] of p - key) )
               set result fput p result]    ;we add the turtle as possible infector
           ]

           ]
         ]
         ;set result sentence temp-val result
                                       ;
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end

to-report collect-all-possible-infecting-turtles-at [tick-nb]     ;duplicate of -- has-met-all-possible-infector -- whether app or not - filter later thos who have app and are push
  let t tick-nb - (15 * day-duration)  ;desease-duration-ticks            ;for immune only --
  let detection-backpropagation 15 * day-duration

  let result []

  foreach ( table:keys has-met-dict ) [ ; all met-array at key ticks => problem is to select only those that are detected later in time,
                                        ; but according to a certain period (maximum period of infection (20 days first)
    [key] ->

      if key <= t
        [
         let met-at-key table:get has-met-dict key
         let temp-val 0
         ;either 0 => at detection-time, hence no report possible  //  should collect up to ? no  will be available later...
         ;=> undetected /
         ;at insertion... key is when meeting - the met turtle p must not have its detection time different than 0 OR must be > key since once detected turtles met are no longer logged
         ;not so simple = main problem is meeting again undetected ?   not really but could be the bug
         foreach met-at-key [p ->
           if [detection-time] of p > 0
           [
             if [detection-time] of p < (15 * day-duration + key) ;and [detection-time] of p
               [
               print (word " det (under 15 days APP or NOT): " [detection-time] of p " key " key " diff " ([detection-time] of p - key) )
               set result fput p result]    ;we add the turtle as possible infector
           ]

           ]
         ]
         ;set result sentence temp-val result
                                       ;
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end


to-report collect-all-possible-infecting-turtles-app-at [tick-nb]     ;duplicate of -- has-met-all-possible-infector -- whether app or not - filter later thos who have app and are push
  let t tick-nb - (15 * day-duration)   ;desease-duration-ticks            ;for immune only --
  let detection-backpropagation 15 * day-duration

  let result []

  foreach ( table:keys has-met-dict ) [ ; all met-array at key ticks => problem is to select only those that are detected later in time,
                                        ; but according to a certain period (maximum period of infection (20 days first)
    [key] ->

      if key <= t
        [
         let met-at-key table:get has-met-dict key
         let temp-val 0

         foreach met-at-key [p ->     ; {{table: [[13 [(turtle 196)]] [19 [(turtle 67)]] [27 [(turtle 464)]]      p is a tick !!!
           if [detection-time] of p > 0 and [app-mode] of p = 2
           [
             if [detection-time] of p < (15 * day-duration + key) ;and [detection-time] of p
               [
               print (word " det2 (under 15 days APP PUSH): " [detection-time] of p " key " key " diff " ([detection-time] of p - key) )
               set result fput p result]    ;we add the turtle as possible infector
           ]

           ]
         ]
         ;set result sentence temp-val result
                                       ;
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end

to insp
 print ""
 let val one-of  turtles with [app-mode = 2 and health-state != 0 and detection-time > 0]
 ;inspect val
 print "do not use"
 print (word val " at " [infection-ticks-nb] of val " det " [detection-time] of  val " by " [infection-by] of val  " det " [detection-time] of ([infection-by] of val) )

 print [collect-possible-infecting-turtles-at 200] of val
 print [collect-possible-infecting-turtles-at 300] of val
 print [collect-possible-infecting-turtles-at 400] of val
 print [collect-possible-infecting-turtles-at 500] of val
 print [collect-possible-infecting-turtles-at 600] of val
 print [collect-possible-infecting-turtles-at 700] of val
 print [collect-possible-infecting-turtles-at ticks ] of val

 print ""
 ;print [collect-all-possible-infecting-turtles-at 500] of val
 ;print [collect-all-possible-infecting-turtles-at 700] of val
 print [collect-all-possible-infecting-turtles-at ticks ] of val

end


to insp2
 print ""
 let val one-of  turtles with [app-mode > 0 and health-state != 0]
 ;inspect val
 print "app and infected"
 print (word val " at " [infection-ticks-nb] of val " det " [detection-time] of  val " by " [infection-by] of val  " det " [detection-time] of ([infection-by] of val) )

print [infection-ticks-nb + 50] of  val
 print [collect-possible-infecting-turtles-at [infection-ticks-nb + 50] of  val ] of val

 print ""
 ;print [collect-all-possible-infecting-turtles-at 500] of val
 ;print [collect-all-possible-infecting-turtles-at 700] of val
 print [collect-all-possible-infecting-turtles-at ticks ] of val
 print [collect-all-possible-infecting-turtles-app-at ticks ] of val
end

to insp3
 print ""
 let val one-of  turtles with [app-mode > 0 and health-state = 0]
 ;inspect val
 print "app and safe"
 print (word val " at " [infection-ticks-nb] of val " det " [detection-time] of  val " by " [infection-by] of val )

print [infection-ticks-nb + 50] of  val
 print [collect-possible-infecting-turtles-at [infection-ticks-nb + 50] of  val ] of val

 print ""
 ;print [collect-all-possible-infecting-turtles-at 500] of val
 ;print [collect-all-possible-infecting-turtles-at 700] of val
 print [collect-all-possible-infecting-turtles-at ticks ] of val
 print [collect-all-possible-infecting-turtles-app-at ticks ] of val
end





to-report pb
   let result []
   ;print " 1 "
   ask one-of turtles with [app-mode = 2 and health-state != 0]
     [
        ;print " 2 "
        let poss collect-possible-infecting-turtles-at ticks
        ;print poss
        ;print not empty? poss
        if not empty? poss
          [
          ;print " 3 "
          set result sentence poss result
          print (word result " self " self " poss " poss)
          ]
     ]

   ;let val one-of turtles with [ ]
   report result
 ;print [collect-possible-infecting-turtles-at ticks ] of val

end

to-report collect-all-possible-infecting-turtles   ;at the end of the report    call -at
  ;foreach t range 0 ticks collect:    collect-possible-infecting-turtles t
  let result-indicator table:make   ;global... or calculated at the end ?     sinon update declenché a partir de chaque has-met input...
  ;if temporary, it updates until ticks + duration-desease-tick    FIGE - UPDATING tick1 -> value1, tick2 -> value2 (1 updated) ...  record the max ticks for each potential infector
  ;be careful KISS
  ;the set of possibile infecting - the maxes (those detected) modulated by users met eventually
  ;for the indcator, at first what matter is collect all detected infected in a reasonable timeframe, then collect all of these on each past points (as it potentially can be an infection that
  ;was not detected at all - asymptomatic or very small symptoms (indetermination of the state). due to the covid-19 specifiiecities, these two value are of complmentary value.

  ; the runnning mean of size duration-desease-infectisity could be a pertinent indicator

  let result []
  let detection-backpropagation 20 * day-duration

  ;foreach range day-duration ticks day-duration        ;top
  foreach (range 1 ticks day-duration)        ;update indicator avec value 00000012100003421....  surement plus etalé mais pas sur + cumulative 0000134444447AAAAAA
    [
      tic -> set temp collect-possible-infecting-turtles-at tic

    ;ICI   COOL je crois =>



    ;indicator add temp (ensure return 0 if...)....   300j * 1000 tortue   juste la somme mais debug


;TBD2 test afficahge dans debug   a mi chemin

;        let val table:get has-met-dict key
;         ;either 0 => at detection-time, hence no report possible  //  should collect up to ? no  will be available later...
;         ;=> undetected /
;         let temp-val filter [p -> [detection-time] of p > 0 and  [detection-time] of p - key < 20] val   ;use 20 as veriable like backpropagation-time
;         set result sentence temp-val result

 ;   ]


  ]

  ;report sort reduce sentence table:values has-met-dict
  report result

end

to info-indicator  ;to debug  / only last 15 days prior to detection ?
                            ;but what for people that got immune and undetected... we keep all life ?
                            ;we can imagine that tests confirm positivity or not - useful for actual
  let t-min ticks - (20 * day-duration)
  let met-20d has-met-since t-min
  let met-all has-met-since 0


  print " "
  print (word "=== START contamination indicator analysis for turtle " self " after " print-time ticks)  ;length met met
  let app "no"
  if app-mode != 0 [ifelse app-mode = 1 [set app "yes pull"][set app "yes push"]]
  print (word "app: " app )
  ifelse infection-start-time = 0
    [print "not infected"]
    [
      print (word self " was infected-by " infection-by " at " print-time infection-start-time)
      print (word self " has infected " has-infected-dict)
      ifelse detection-time = 0
        [print "not detected"]
        [
          print (word " was detected at: " print-time detection-time " ie. " print-time (detection-time - infection-start-time) " after infection")
          print (word " was infected " print-time (ticks - infection-start-time) " ago and detected " print-time (ticks - detection-time) " ago")

          ;print (word "GUESS: infection date between  " print-time (detection-time - (3 / 4 * desease-duration-ticks)) " and " print-time detection-time)
          print (word "GUESS: infection date between  " print-time (detection-time - (20 * day-duration)) " and " print-time detection-time)

        ]   ;" contagious: " contagious
    ]
   print " "
   ;either infected or not => I partition on detection now + app user / contributors

  print "Known and estimated information (for instance we can't know all person we have met - the simulation can"
  print ""
  print "Numbers (real)"
  print (word length met-all " met turtle points (a turtle can be met several times)"  met-all)
  let t table:counts sort met-all
  print (word table:length t " met turtles (grouped)"  t)

  print ""
  print "Numbers (app inference)"

;;TODO here -  rajout et test des choses significatives... variation param
; met / met(infected) / met(detected)    => surtout met detected push  ...> aussi pull car le rapport tend vers la realité

  ifelse detection-time = 0
    [ ;undetected / worst cases as we don't know if healthy, sick or immune => this is the classic use case / we want the app to predict
      ;the last 15d can be associated to eventual sick state (current)
      ;the the rest can be related to being immune, ie. infected prior the 15d before detection.

      ;we're interested in met person that will be detected later and that are pushing information.
      ;if undetected, we know "nothing"... we could know that such met turtle was not confirmed sick/immune but all output are possible (and is the majority)

      ;detected are sick people being detected, later do detected-immune (for tests)

      ;find met people that were detected less than 15 days after they met me




;      set t filter [p -> [app-mode] of p = 2] met-all
;      print (word "met-push " length t t)
;
;      set t filter [p -> [app-mode] of p = 1] met-all
;      print (word "met-pull " length t t)
;
;      set t filter [p -> member? [app-mode] of p [1 2]] met-all   ;users (all having app => those concerned by the estimation)
;      print (word "met-users (push-pull) " length t t)
;
;      set t filter [p -> [app-mode] of p = 0] met-all
;      print (word "met-no-app " length t t)



    ]
    [ ;detected / simples cases - but unknown time of infection => estiamation ?   can be today, yesterday, ... up to ? around 15 days...
      ;useful to try to predict infection time - in the last 3/4 duration
      print (word "as detected, show met turtle for the last 20 days before detection - must contain infector)")

      print ""
      let all has-met-from-to infection-start-time detection-time
      print (word length all " met turtles (ALL) between infection and detection " all)
      set t filter [p -> member? [app-mode] of p [1 2] ] all
      print (word " dont " length t " app users (PULL+PUSH) " t)
      set t filter [p -> [app-mode] of p = 1 ] all
      print (word "   " length t " app simple users (PULL only) " t)
      set t filter [p -> [app-mode] of p = 2 ] all
      print (word " et " length t " app contributors (PUSH only) " t)

   ;ICI
      set t filter [p -> [app-mode] of p = 2 and [detection-time] of p > 0 ] all
      print (word " dont " length t " PERSONNES DETECTEES" t)
      set t filter [p -> [infection-start-time] of p > 0 and [infection-start-time] of p <= detection-time ] all
      print filter [p -> detection-time - [infection-start-time] of p  < desease-duration-ticks ] all
      print (word "   amongst " length t " (real met infected omni)" t)

;TO DEBUG

      set t filter [p -> [infection-start-time] of p > 0 and [infection-start-time] of p <= detection-time ] all
      print filter [p -> detection-time - [detection-time] of p  > 0 ] all

      ;use infection-end-time

      print (word "   amongst " length t " (real met infected undetected)" t)



      print ""
      let all-estim has-met-from-to (detection-time - (15 * day-duration)) detection-time
      print (word length all-estim " met turtles (ALL for 15 days) between (ESTIMATED) infection and detection " all-estim)
      set t filter [p -> member? [app-mode] of p [1 2] ] all-estim
      print (word " dont " length t " app users (PULL+PUSH) " t)
      set t filter [p -> [app-mode] of p = 1 ] all-estim
      print (word "   " length t " app simple users (PULL only) " t)
      set t filter [p -> [app-mode] of p = 2 ] all-estim
      print (word "   " length t " app contributors (PUSH/PULL) " t)

      set t filter [p -> [app-mode] of p = 2 and [detection-time] of p > 0 ] all-estim
      print (word " dont " length t " PERSONNES DETECTEES" t)

      ;TO DO => filter n days before detection
      ;set t filter [p -> [detection-time] of p > 0 ] met
      ;set t filter [p -> member? [app-mode] of p [1 2] ] all-estim

;  print (word "infected-users-detected " length t t)
;
    ]



;has-met-all-possible-infector




;  set t filter [p -> [health-state] of p > 0] met
;  print (word "infected-current " length t t)
;
;  set t filter [p -> [infection-start-time] of p > 0] has-met-since 0
;  print (word "infected-all " length t t)
;
;  set t filter [p -> [infection-start-time] of p > 0 and [detection-time] of p > 0] has-met-since 0
;  print (word "infected-all-detected " length t t)
;
;  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1 2] ] has-met-since 0
;  print (word "infected-users " length t t)
;
;  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1 2] and [detection-time] of p > 0 ] has-met-since 0
;  print (word "infected-users-detected " length t t)
;
;  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [2] and [detection-time] of p > 0 ] has-met-since 0
;  print (word "infected-users-detected-push " length t t)
;
;  set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1] and [detection-time] of p > 0 ] has-met-since 0
;  print (word "infected-users-detected-pull " length t t)
;
;    set t filter [p -> [infection-start-time] of p > 0 and  member? [app-mode] of p [1] and [detection-time] of p = 0 ] has-met-since 0
;  print (word "infected-users-undetected-pull " length t t)

  ;set t table:counts has-met-dict
  ;table:group-agents has-met-since 0 [ dtection-time > 0 ]
  ;print (word "infected-all-grouped " length t t)

;  set t table:counts sort has-met-since 0   ;   all by frequencies => dict  turtle => met times (lose ticks)
;  print (word "infected-all-grouped " table:length t t)
;
;  set t filter [p -> [detection-time] of p > 0 ] met
;  print (word "detected-real " length t t)

end

to-report undetected-infected-alive-all
   report count turtles with [health-state = -1 and health-state > 0 and detection-time = 0 ]
end

to-report undetected-infected-all
   report count turtles with [(health-state = -2 or health-state >= 0) and detection-time = 0 ]
end

to-report undetected-final
   report count turtles with [health-state = -2 or health-state = 0 and detection-time = 0 ]
end

to-report days
  let d  ticks / day-duration
  report d
end

to-report days-from [tick-nb]
  let d  tick-nb / day-duration
  report precision d 1
end

to-report print-time [time-in-ticks]
  report (word days-from time-in-ticks " days (" time-in-ticks ")")
end

to-report ticks-since-infected
  report ticks - infection-start-time
end

to-report immune?
  report health-state = -1
end

to-report sick?
  report health-state > 0
end

to-report dead?
  report health-state = -2
end

to-report app-user?
  report app-mode > 0
end

to-report app-contrib?
  report app-mode > 1
end

to-report detected?
  report detection-time > 0
end

to startup
  setup-constants ;; so that people-nb can be used as upper bound of people-nb slider
end

;;Indicator analysis
to-report immunized-nb
  report count turtles with [health-state = -1]
end

to-report dead-nb
  report count turtles with [health-state = -2]
end

to-report get-patient-zero
  let p0 0
  ask (turtles  with [infection-by = self]) [set p0 self]
  if interactive [inspect p0]
  report p0
end

to-report desease-duration-ticks
  report desease-duration * day-duration
end

to information
  show "nombre de tortues detectées: "
  show "niveau 5 (total - detected - mean detection times"
  show count turtles  with [health-k = 5]
  show count turtles with [health-k = 5 and detection-time > 0]
  show "niveau 4"
  show count turtles  with [health-k = 4]
  show count turtles with [health-k = 4 and detection-time > 0]
  show "niveau 3"
  show count turtles  with [health-k = 3]
  show count turtles with [health-k = 3 and detection-time > 0]
  show "niveau 2"
  show count turtles  with [health-k = 2]
  show count turtles with [health-k = 2 and detection-time > 0]
  show "niveau 1"
  show count turtles  with [health-k = 1]
  show count turtles with [health-k = 1 and detection-time > 0]
end

to end-of-simulation-report
  let t 0
  print "-----------------------------------"
  print "------------Résultats--------------"
  print "-----------------------------------"
  print (word "nombre de ticks de la simulation : "  ticks " soit " days-from(ticks) " jours" )
  print (word "patient zero : "  patient-zero)
  print (word  "-nb décés: " count turtles  with [health-state = -2] " sur " people-nb " soit: " precision ((count turtles  with [health-state = -2]) / count turtles * 100) 2 "%")
  print (word "-immunisés: " (count turtles  with [health-state = -1]) " sur " people-nb " soit: " precision ((count turtles  with [health-state = -1]) / count turtles * 100) 2 "%")
  print (word "-non-infectés: " (count turtles  with [health-state = 0]) " sur " people-nb " soit: " precision ((count turtles  with [health-state = 0]) / count turtles * 100) 2 "%")
  print (word "infection moyenne R0: " precision mean [ length has-infected-flat  ] of turtles with [ health-k  >= 0] 3)
  print "-----------------------------------"
  print "------Liste (moyenne min max)------"
  print "-----------------------------------"
  set t [ length reduce [ [a b] -> sentence a b  ] table:values has-met-dict  ] of turtles
  print (word "rencontre de personnes en moyenne: " precision mean t 2  " min: " min t  " max " max t)
  set t [ length has-infected-flat  ] of turtles with [ health-k  >= 0] ;not good  table:length has-infected-dict
  print (word "nombre de personnes infectées en moyenne (R0): "  precision mean t 3  " min: " min t " max " max t)
    set t [ length has-infected-flat  ] of turtles with [ health-k  > 0] ;not good  table:length has-infected-dict
  print (word "nombre de personnes infectées en moyenne (R0): "  precision mean t 3  " min: " min t " max " max t)
  print ""
;  set t [ length reduce [ [a b] -> sentence a b  ] table:values has-met-dict  ] of turtles with [health-k >= 0]
;  print (word "rencontre de personnes infectées en moyenne (faux): "  mean t  " min: " min t " max " max t)
;  print "TO DO personnes rencontrées once infected"
;  print "TO DO personnes infectées rencontrées"
;
;  print "rencontre de personnes PUSH: "
;  print "rencontre de personnes PUSH infectées: "
;  print "rencontre de personnes état grave - cat 5: "
  print "-----------------------------------"
  print "------Catégories selon santé-------"
  print "-----------------------------------"

  print (word "non infectées: " (count turtles  with [health-state = 0]) " soit " precision ((count turtles  with [health-state = 0]) / count turtles * 100) 2 "% ")
  print (word "Asymptomatique: (k0): " (count turtles  with [health-k = 0 and health-state != 0]) " soit " precision ((count turtles  with [health-k = 0 and health-state != 0]) / count turtles * 100) 2 "% ")


  print (word "non detectées (hors asymp) : " (count turtles  with [health-k != 0 and detection-time = 0]) "/" (count turtles  with [health-k != 0 ]) " soit " precision ((count turtles  with [health-k != 0 and detection-time != 0]) / (count turtles  with [health-k != 0 ]) * 100) 2 "% ")
  print (word "non detectées (+ asymp) : " (count turtles  with [health-state != 0 and detection-time = 0]) "/" (count turtles with [health-state != 0 ]) " soit " precision ((count turtles  with [health-state != 0 and detection-time = 0]) / (count turtles  with [health-state != 0 ]) * 100) 2 "% ")

  print (word "Asymptomatiqu (k0): " (count turtles  with [health-k = 0 and health-state != 0]) " soit " precision ((count turtles  with [health-k = 0 and health-state != 0]) / count turtles * 100) 2 "% ")
  print ""
  print (word "symptomatique k1: " count turtles  with [health-k = 1] " dont " count turtles with [health-k = 1 and detection-time > 0] " détectées")
  set t [detection-time - infection-ticks-nb] of turtles with [health-k = 1 and detection-time > 0]
  if not empty? t [print (word "  temps de detection moyen: " round days-from(mean t) "(" round mean t ") min: " days-from(min t) "(" min t ") max: " days-from(max t) "(" max t ")")]
  print (word "symptomatique k2: " count turtles  with [health-k = 2] " dont " count turtles with [health-k = 2 and detection-time > 0] " détectées")
  set t [detection-time - infection-ticks-nb] of turtles with [health-k = 2 and detection-time > 0]
  if not empty? t [print (word "  temps de detection moyen: " round days-from(mean t) "(" round mean t ") min: " days-from(min t) "(" min t ") max: " days-from(max t) "(" max t ")")]
  print (word "symptomatique k3: " count turtles  with [health-k = 3] " dont " count turtles with [health-k = 3 and detection-time > 0] " détectées")
  set t [detection-time - infection-ticks-nb] of turtles with [health-k = 3 and detection-time > 0]
  if not empty? t [print (word "  temps de detection moyen: " round days-from(mean t) "(" round mean t ") min: " days-from(min t) "(" min t ") max: " days-from(max t) "(" max t ")")]
  print (word "symptomatique k4: " count turtles  with [health-k = 4] " dont " count turtles with [health-k = 4 and detection-time > 0] " détectées")
  set t [detection-time - infection-ticks-nb] of turtles with [health-k = 4 and detection-time > 0]
  if not empty? t [print (word "  temps de detection moyen: " round days-from(mean t) "(" round mean t ") min: " days-from(min t) "(" min t ") max: " days-from(max t) "(" max t ")")]
  print (word "symptomatique k5: " count turtles  with [health-k = 5] " dont " count turtles with [health-k = 5 and detection-time > 0] " détectées")
  set t [detection-time - infection-ticks-nb] of turtles with [health-k = 5 and detection-time > 0]
  if not empty? t [print (word "  temps de detection moyen: " round days-from(mean t) "(" round mean t ") min: " days-from(min t) "(" min t ") max: " days-from(max t) "(" max t ")")]

end


;INDICATORS - met - infected etc...
to-report max-met-turtle
  ;ask turtles [analyze-turtle [self]]
  let maxi 0
  let result turtle 1
  ;foreach (list turtles) [t -> if analyze-turtle t > analyze-turtle maxi  [set maxi t ] ]
  foreach sort turtles [ t ->
  ask t [
    let all has-met-flat
    if length all > maxi
        [set maxi length all
        set result t]
  ]
]
  report result    ;show analyze-turtle turtle maxi  returns the number
end

to-report has-met-flat   ;flatten
  report sort reduce sentence table:values has-met-dict
end

to-report has-met-since [t]  ;flatten
  let result []
  foreach ( table:keys has-met-dict ) [
    [key] -> if key >= t   ;and logically < ticks
       [
         let val table:get has-met-dict key
         set result sentence val result
       ]
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end

to-report has-met-from-to [t1 t2]  ;flatten
  let result []
  foreach ( table:keys has-met-dict ) [
    [key] -> if key >= t1 and key <= t2   ;and logically < ticks
       [
         let val table:get has-met-dict key
         set result sentence val result
       ]
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end

to-report has-met-all-possible-infector
  let t ticks - desease-duration-ticks ;for immune only
  let result []
  foreach ( table:keys has-met-dict ) [
    [key] -> if key >= t   ;and logically < ticks
       [
         let val table:get has-met-dict key

         let temp-val filter [p -> [detection-time] of p - key < 20] val

         set result sentence temp-val result
       ]
  ]
  ;report sort reduce sentence table:values has-met-dict
  report result
end

to-report max-infecting-turtle
  ;ask turtles [analyze-turtle [self]]
  let maxi 0
  let result turtle 1
  ;foreach (list turtles) [t -> if analyze-turtle t > analyze-turtle maxi  [set maxi t ] ]
  foreach sort turtles [ t ->
  ask t [
    let all has-infected-flat
    if length all > maxi
        [set maxi length all
        set result t]
  ]
]
  report result    ;show analyze-turtle turtle maxi  returns the number
end

to-report has-infected-flat   ;flatten
  let x (sentence table:values has-infected-dict)
  ifelse  length x > 0
    [report sort reduce sentence table:values has-infected-dict]  ;with [table:length has-infected-dict > 0]
    [report [] ]
end


;;;TO DO personnes rencontrées once infected"
;;;TO DO personnes infectées rencontrées"

to-report had-met-infected
  ;;pas simple... si seulement filtrer les met si status k >
  ;;si non detecté ??  que faire ?
  ;; en theorie, infecte de infection-start à infection-end (ou end of desease for now)
  ;; les non detectés doivent etre gere differement ptet...
  ;; potential-met-infected  différent de met-infected

  ;; pour tous les rencontree   group par healtk apres...
  ;;; si infectieux then keep

;ICIIII  ;;infectieux => detection = 0 and  infection-start-time + ticks < 3/4*duration-cesease.
;  has-met-dict keysAndValuesDo: []
  if contagious
      [  ;contagious

      ]
  ;turtles infection-start-time

  ;has-met-dict having app-mode = 2

end

to-report copy-table [ orig ]
  let copy 0
  foreach ( table:keys orig ) [
    [key] -> table:put copy key ( table:get orig key )
  ]
  report copy
  end

;;show table:group-items range 10 [ num -> num mod 3 ]  par health-k    par time ticks difference


to-report contagious
   ifelse infection-start-time > 0
    [
      ifelse  detection-time = 0
        [report infection-start-time + ticks < 3 * desease-duration / 4]
        [ report false]]
    [print "ZERO CONTAGIOUS"
    report false]


end



; grouping - can be useful

to-report has-met-groups
  report table:counts has-met-flat
end

to-report has-infected-groups
  report table:counts has-infected-flat
end

;;had-met-infected & had-met-infected-flat...
;;calculated now.




;;;;;;;;;OLDER versions

to-report has-infected [one-turtle]  ;flatten
  report sentence table:values one-turtle has-infected-dict
end

to-report has-met [one-turtle]  ;flatten
  report sentence table:values one-turtle has-met-dict
end

;;;
to-report had-met-mean
  report mean    [ length reduce [ [a b] -> sentence a b  ] table:values has-met-dict  ] of turtles
end

;;;desactivated below

to-report xhad-met-infected-mean
  ;mean [count has-met with [health-state > 0]] of turtles with [health-state < 0]
  report 1; mean    [ length reduce [ [a b] -> sentence a b  ] [table:values has-met-dict] of turtles with [health-state < 0]  ] of turtles
end

to-report  xhas-infected-mean
  report 1; mean [flatten-length has-infected] of turtles with [health-state < 0]
end

to-report  xhas-infected-max
  report 1;max [flatten-length has-infected] of turtles with [health-state < 0]
end

to-report  xhas-infected-min
  report 1;min [flatten-length has-infected] of turtles with [health-state < 0]
end

to-report xflatten-length [nested]  ;use sentece instead !
  let l  0
  foreach nested [array -> set l  l + length array]
  report l
end
@#$#@#$#@
GRAPHICS-WINDOW
390
10
1408
669
-1
-1
10.0
1
10
1
1
1
0
1
1
1
-50
50
-32
32
1
1
1
ticks
30.0

SLIDER
5
65
180
98
proba-infection
proba-infection
0.0
99.0
20.0
1.0
1
%
HORIZONTAL

BUTTON
210
10
265
45
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
330
10
385
46
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
0

PLOT
15
230
285
390
covid-dist-app
days
people
0.0
120.0
0.0
200.0
true
true
"" ""
PENS
"sick" 1.0 0 -2674135 true "" "plotxy days count turtles with [ sick? ]"
"immune" 1.0 0 -10899396 true "" "plotxy days count turtles with [ immune? ]"
"healthy" 1.0 0 -7500403 true "" "plotxy days count turtles with [ not sick? and not immune? ]"
"dead" 1.0 0 -16777216 true "" "plotxy days count turtles with [ dead?]"

SLIDER
5
10
180
43
people-nb
people-nb
10
5000
759.0
1
1
NIL
HORIZONTAL

MONITOR
185
455
250
500
NIL
%infected
1
1
11

MONITOR
315
455
380
500
NIL
%immune
1
1
11

MONITOR
300
240
370
285
days
ticks / day-duration
1
1
11

BUTTON
265
10
320
45
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
120
180
153
proba-symptom
proba-symptom
0
100
100.0
1
1
%
HORIZONTAL

SLIDER
205
120
377
153
proba-severe
proba-severe
0
99
10.0
1
1
%
HORIZONTAL

TEXTBOX
10
105
160
123
once infected
11
0.0
1

SLIDER
195
175
385
208
desease-duration-max
desease-duration-max
0
50
20.0
1
1
days
HORIZONTAL

SLIDER
5
175
190
208
desease-duration-min
desease-duration-min
0
30
20.0
1
1
days
HORIZONTAL

TEXTBOX
10
50
220
76
global chance of beeing infected
11
0.0
1

TEXTBOX
10
160
160
178
desease characteristics
11
0.0
1

TEXTBOX
180
130
205
148
 =>
11
0.0
1

TEXTBOX
15
390
550
416
=============== APP parameters ===============
11
0.0
1

SLIDER
5
420
177
453
app-pull-users
app-pull-users
0
100
50.0
1
1
%
HORIZONTAL

SLIDER
205
420
377
453
app-push-users
app-push-users
0
100
50.0
1
1
%
HORIZONTAL

SWITCH
5
500
140
533
simulation-app
simulation-app
0
1
-1000

TEXTBOX
255
405
405
423
contributeur
11
0.0
1

TEXTBOX
65
405
215
423
read-only
11
0.0
1

MONITOR
215
500
285
545
user (pull)
%app-user
17
1
11

MONITOR
285
500
360
545
user (push)
%app-contrib
17
1
11

MONITOR
250
455
315
500
NIL
%dead
17
1
11

MONITOR
300
290
370
335
detected
%detected
17
1
11

TEXTBOX
15
215
380
241
=========================== detection =======
11
0.0
1

SWITCH
5
460
142
493
debug-mode
debug-mode
1
1
-1000

PLOT
425
280
650
400
Death
days
people
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy days count turtles with [ dead?]"

MONITOR
300
340
370
385
deaths
%dead-inc
17
1
11

PLOT
425
160
650
280
Death-inc
days
people
0.0
10.0
0.0
3.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [ health-state = -2 and (ticks - infection-end-time <= 2 * day-duration)  ] "

BUTTON
230
60
292
93
1
setup1
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
300
60
362
93
2
setup2
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1025
10
1395
230
max-health-degradation
time
health
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot max [ current-health-val ] of turtles  with [health-state = 2] "
"pen-2" 1.0 0 -13345367 true "" "plot max [ current-health-val ] of turtles  with [ health-k = 1] "
"pen-3" 1.0 0 -1184463 true "" "plot max [ current-health-val ] of turtles  with [ health-k = 2] "
"pen-4" 1.0 0 -955883 true "" "plot max [ current-health-val ] of turtles  with [ health-k = 3] "
"pen-5" 1.0 0 -2674135 true "" "plot max [ current-health-val ] of turtles  with [ health-k = 4] "
"pen-6" 1.0 0 -5825686 true "" "plot max [ current-health-val ] of turtles  with [ health-k = 5] "

PLOT
1060
10
1430
230
5-samples-symptoms - each 50 ticks
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot [ current-health-val ] of sample-1"
"pen-1" 1.0 0 -1184463 true "" "plot [ current-health-val ] of sample-2"
"pen-2" 1.0 0 -955883 true "" "plot [ current-health-val ] of sample-3"
"pen-3" 1.0 0 -2674135 true "" "plot [ current-health-val ] of sample-4"
"pen-4" 1.0 0 -5298144 true "" "plot [ current-health-val ] of sample-5"

PLOT
1105
10
1430
210
5-samples-current-health-state
NIL
NIL
0.0
10.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot [ current-health-state ] of sample-5"
"pen-1" 1.0 0 -955883 true "" "plot [ current-health-state ] of sample-4"
"pen-2" 1.0 0 -1184463 true "" "plot [ current-health-state ] of sample-3"
"pen-3" 1.0 0 -11221820 true "" "plot [ current-health-state ] of sample-2"
"pen-4" 1.0 0 -4528153 true "" "plot [ current-health-state ] of sample-1"

@#$#@#$#@
## ACKNOWLEDGMENT

This model is an alternate visualization of the Virus model from the Biology section of the NetLogo Models Library. It uses visualization techniques as recommended in the paper:

* Kornhauser, D., Wilensky, U., & Rand, W. (2009). Design guidelines for agent based model visualization. Journal of Artificial Societies and Social Simulation (JASSS), 12(2), 1. http://ccl.northwestern.edu/papers/2009/Kornhauser,Wilensky&Rand_DesignGuidelinesABMViz.pdf.

## WHAT IS IT?

This model simulates the transmission and perpetuation of a virus in a human population. The circle visualization in this model makes it easier to see when agents interact.

Ecological biologists have suggested a number of factors which may influence the survival of a directly transmitted virus within a population. (Yorke, et al. "Seasonality and the requirements for perpetuation and eradication of viruses in populations." Journal of Epidemiology, volume 109, pages 103-123)

## HOW IT WORKS

The model is initialized with 150 people, of which 10 are infected.  People move randomly about the world in one of three states: healthy but susceptible to infection (green), sick and infectious (red), and healthy and immune (gray). People may die of infection or old age.  When the population dips below the environment's "carrying capacity" (set at 300 in this model) healthy people may produce healthy (but susceptible) offspring.

Some of these factors are summarized below with an explanation of how each one is treated in this model.

### The density of the population

Population density affects how often infected, immune and susceptible individuals come into contact with each other. You can change the size of the initial population through the NUMBER-PEOPLE slider.

### Population turnover

As individuals die, some who die will be infected, some will be susceptible and some will be immune.  All the new individuals who are born, replacing those who die, will be susceptible.  People may die from the virus, the chances of which are determined by the slider CHANCE-RECOVER, or they may die of old age.

In this model, people die of old age at the age of 50 years.  Reproduction rate is constant in this model.  Each turn, if the carrying capacity hasn't been reached, every healthy individual has a 1% chance to reproduce.

### Degree of immunity

If a person has been infected and recovered, how immune are they to the virus?  We often assume that immunity lasts a lifetime and is assured, but in some cases immunity wears off in time and immunity might not be absolutely secure.  In this model, immunity is secure, but it only lasts for a year.

### Infectiousness (or transmissibility)

How easily does the virus spread?  Some viruses with which we are familiar spread very easily.  Some viruses spread from the smallest contact every time.  Others (the HIV virus, which is responsible for AIDS, for example) require significant contact, perhaps many times, before the virus is transmitted.  In this model, infectiousness is determined by the INFECTIOUSNESS slider.

### Duration of infectiousness

How long is a person infected before they either recover or die?  This length of time is essentially the virus's window of opportunity for transmission to new hosts. In this model, duration of infectiousness is determined by the DURATION slider.

### Hard-coded parameters

Four important parameters of this model are set as constants in the code (See `setup-constants` procedure). They can be exposed as sliders if desired. The turtles’ lifespan is set to 50 years, the carrying capacity of the world is set to 300, the duration of immunity is set to 52 weeks, and the birth-rate is set to a 1 in 100 chance of reproducing per tick when the number of people is less than the carrying capacity.

## HOW TO USE IT

Each "tick" represents a week in the time scale of this model.

The INFECTIOUSNESS slider determines how great the chance is that virus transmission will occur when an infected person and susceptible person occupy the same patch.  For instance, when the slider is set to 50, the virus will spread roughly once every two chance encounters.

The DURATION slider determines the number of weeks before an infected person either dies or recovers.

The CHANCE-RECOVER slider controls the likelihood that an infection will end in recovery/immunity.  When this slider is set at zero, for instance, the infection is always deadly.

The SETUP button resets the graphics and plots and randomly distributes NUMBER-PEOPLE in the view. All but 10 of the people are set to be green susceptible people and 10 red infected people (of randomly distributed ages).  The GO button starts the simulation and the plotting function.

The TURTLE-SHAPE chooser controls whether the people are visualized as person shapes or as circles.

Three output monitors show the percent of the population that is infected, the percent that is immune, and the number of years that have passed.  The plot shows (in their respective colors) the number of susceptible, infected, and immune people.  It also shows the number of individuals in the total population in blue.

## THINGS TO NOTICE

The factors controlled by the three sliders interact to influence how likely the virus is to thrive in this population.  Notice that in all cases, these factors must create a balance in which an adequate number of potential hosts remain available to the virus and in which the virus can adequately access those hosts.

Often there will initially be an explosion of infection since no one in the population is immune.  This approximates the initial "outbreak" of a viral infection in a population, one that often has devastating consequences for the humans concerned. Soon, however, the virus becomes less common as the population dynamics change.  What ultimately happens to the virus is determined by the factors controlled by the sliders.

Notice that viruses that are too successful at first (infecting almost everyone) may not survive in the long term.  Since everyone infected generally dies or becomes immune as a result, the potential number of hosts is often limited.  The exception to the above is when the DURATION slider is set so high that population turnover (reproduction) can keep up and provide new hosts.

## THINGS TO TRY

Think about how different slider values might approximate the dynamics of real-life viruses.  The famous Ebola virus in central Africa has a very short duration, a very high infectiousness value, and an extremely low recovery rate. For all the fear this virus has raised, how successful is it?  Set the sliders appropriately and watch what happens.

The HIV virus, which causes AIDS, has an extremely long duration, an extremely low recovery rate, but an extremely low infectiousness value.  How does a virus with these slider values fare in this model?

## EXTENDING THE MODEL

Add additional sliders controlling the carrying capacity of the world (how many people can be in the world at one time), the average lifespan of the people and their birth-rate.

Build a similar model simulating viral infection of a non-human host with very different reproductive rates, lifespans, and population densities.

Add a slider controlling how long immunity lasts. You could also make immunity imperfect, so that immune turtles still have a small chance of getting infected. This chance could get higher over time.

## VISUALIZATION

The circle visualization of the model comes from guidelines presented in
Kornhauser, D., Wilensky, U., & Rand, W. (2009). http://ccl.northwestern.edu/papers/2009/Kornhauser,Wilensky&Rand_DesignGuidelinesABMViz.pdf.

At the lowest level, perceptual impediments arise when we exceed the limitations of our low-level visual system. Visual features that are difficult to distinguish can disable our pre-attentive processing capabilities. Pre-attentive processing can be hindered by other cognitive phenomena such as interference between visual features (Healey 2006).

The circle visualization in this model is supposed to make it easier to see when agents interact because overlap is easier to see between circles than between the "people" shapes. In the circle visualization, the circles merge to create new compound shapes. Thus, it is easier to perceive new compound shapes in the circle visualization.
Does the circle visualization make it easier for you to see what is happening?

## RELATED MODELS

* HIV
* Virus on a Network

## CREDITS AND REFERENCES

This model can show an alternate visualization of the Virus model using circles to represent the people. It uses visualization techniques as recommended in the paper:

Kornhauser, D., Wilensky, U., & Rand, W. (2009). Design guidelines for agent based model visualization. Journal of Artificial Societies and Social Simulation, JASSS, 12(2), 1.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (1998).  NetLogo Virus - Circle Visualization model.  http://ccl.northwestern.edu/netlogo/models/Virus-CircleVisualization.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1998 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227.

<!-- 1998 -->
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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count turtles with [health-state = -1]</metric>
    <metric>count turtles with [health-state = -2]</metric>
    <metric>mean [length has-met] of turtles with [health-state &lt; 0]</metric>
    <metric>mean [length has-infected] of turtles with [health-state &lt; 0]</metric>
    <metric>max [length has-infected] of turtles with [health-state &lt; 0]</metric>
    <metric>min [length has-infected] of turtles with [health-state &lt; 0]</metric>
    <enumeratedValueSet variable="proba-severe">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desease-duration-max">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="app-push-users">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proba-symptom">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-app">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="people-nb">
      <value value="1000"/>
      <value value="1250"/>
      <value value="1500"/>
      <value value="1750"/>
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desease-duration-min">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-mode">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proba-infection">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="app-pull-users">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp2" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>immunized-nb</metric>
    <metric>dead-nb</metric>
    <metric>had-met-mean</metric>
    <metric>had-met-infected-mean</metric>
    <metric>has-infected-mean</metric>
    <metric>has-infected-min</metric>
    <metric>has-infected-max</metric>
    <enumeratedValueSet variable="proba-severe">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desease-duration-max">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="app-push-users">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proba-symptom">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-app">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="people-nb">
      <value value="1000"/>
      <value value="1500"/>
      <value value="2000"/>
      <value value="2500"/>
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desease-duration-min">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-mode">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proba-infection">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="app-pull-users">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
