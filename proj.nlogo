extensions [palette]
__includes [ "a_star.nls" ]

globals [
  ; num-drivers ;; the number of drivers that will be working - now is an input

  min-ride-distance ; the minimum distance of a ride

  next-arrival-time ; the tick at which the next ride will be created

  ; variables for statistics
  num-drivers-starting
  num-drivers-occupied
  num-rides
  num-rides-taken
  num-clients-gave-up

  ; money constants (now being set by their inputs' values in "setup-globals")
  B ; ride base price
  P ; price charged per distance unit of ride
  driver-salary
  ; money statistics
  income
  expenses

  ;; agentsets
  roads ;; agentset containing the patches that are roads

  test-start
]

turtles-own [
  starting? ;; curently going to start a ride (moving to the start)
  occupied? ;; currently in a ride
  current-ride-start ;; patch representing a start of ride that we are working on

  current-path ;; calculated path currently being traversed
  current-ride-profit ;; the profit of the current ride based on its duration
]

patches-own [
  is-taken? ;; is the ride already taken by a car?
  is-client? ;; is a start of ride
  is-tapped? ;; a start of a ride that was touched by a driver that was assigned to this ride
  ride-created-tick ;; the tick at which the ride was created
  ride-expire-tick ;; the tick at which the ride will expire
  is-final? ;; is an end of ride?

  my-ride-end ;; the patch that is the ride's end
]

;;;; Setup procedures

; Setup with a default grid
to setup
  clear-all

  setup-a-star

  setup-globals
  setup-grid-map
  setup-patches
  setup-drivers

  record-driver-data
  record-ride-data
  reset-ticks

  ; Scheduling the first arrival - has to be after "reset-ticks"
  schedule-arrival
end

; Setup with an imported image
to setup-with-image [user-image]
  clear-all

  setup-a-star

  setup-globals
  setup-image-map user-image
  setup-patches
  setup-drivers

  record-driver-data
  record-ride-data
  reset-ticks

  ; Scheduling the first arrival - has to be after "reset-ticks"
  schedule-arrival
end

to setup-globals
  set min-ride-distance 5

  set num-drivers-starting 0
  set num-drivers-occupied 0
  set num-rides 0
  set num-rides-taken 0
  set num-clients-gave-up 0

  ; set B 3
  set B ride-base-price
  ; set P 5
  set P price-per-ride-distance-unit
  ; set driver-salary 0.05
  set driver-salary salary-per-driver-per-tick

  set income 0
  set expenses 0
end

to setup-patches
  ; Resetting to initial patch state
  ask patches [
    set is-client? false
    set is-tapped? false
    set is-final? false
    set is-taken? false
    set my-ride-end nobody
    set ride-created-tick -1
    set ride-expire-tick -1
    set plabel ""
    set plabel-color black
  ]

  ; The roads agentset is the patches that are white
  set roads patches with [pcolor = white]
end

to setup-drivers
  set-default-shape turtles "car"
  create-turtles num-drivers [
    set starting? false
    set occupied? false
    set current-ride-start nobody
    set current-path []
    put-on-empty-road
    set-driver-color
  ]
end

;;;;; queue and scheduling procedures
to schedule-arrival
  set next-arrival-time (ticks + ceiling random-exponential (20 / mean-arrival-rate))
  ; show "Current tick "
  ; show ticks
  ; show "Scheduled for "
  ; show next-arrival-time
end

;;;;; Map creation procedures
;;; In these procedures, the roads must be white,
;;; and the obstacles must be "obstacle-color" (currently brown but might change)
;;; These methods should be invoked in setup, right before "setup-patches"

to setup-grid-map
  let grid-size-x 5
  let grid-size-y 5
  let grid-x-inc world-width / grid-size-x
  let grid-y-inc world-height / grid-size-y

  ; Everything is an obstacle but the roads, which are painted white below
  ask patches [
    set pcolor obstacle-color
  ]

  ask patches with [(floor((pxcor + max-pxcor - floor(grid-x-inc - 1)) mod grid-x-inc) = 0) or
      (floor((pycor + max-pycor) mod grid-y-inc) = 0)] [
      set pcolor white
  ]
end

to setup-image-map [user-image]
  import-pcolors user-image
end

;;;;; ride procedures
; Creates a ride in random coordinates with a minimum distance
to create-ride
  without-interruption [
    let non-rides roads with [not is-client? and not is-final?]
    let ride-start one-of non-rides

    if (random 100 < spawn-in-epicentre-chance) [
      ; spawn near an epicentre
      let selected-epicentre one-of patches with [plabel = "E"]
      if is-patch? selected-epicentre [
        set ride-start min-one-of non-rides [distance selected-epicentre]
      ]
    ]

    let ride-end one-of non-rides with [self != ride-start and (distance ride-start) > min-ride-distance]

    if (is-patch? ride-start and is-patch? ride-end) [
      ask ride-start [
        set pcolor green
        set is-client? true
        set my-ride-end ride-end

        set ride-created-tick ticks
        set ride-expire-tick (ticks + ride-ticks-to-expire)
      ]

      ask ride-end [
        ; set pcolor red ; commented since this was initially for debug and now causes a lot of confusion
        set is-final? true
      ]
    ]

    ; Next ride arrival has to be scheduled regardless of success because if the world fills up
    ; then no further rides will be created once it clears up
    schedule-arrival
  ]
end

; Called by the driver turtle to finish a ride
to close-ride [ride-start]
  ask ride-start [
    set is-client? false
    set is-tapped? false
    set is-final? false
    set is-taken? false

    set ride-created-tick -1
    set ride-expire-tick -1

    ask my-ride-end [
      set is-final? false
      set pcolor white
    ]

    set my-ride-end nobody
    set pcolor white
  ]
end

; When a ride expires, cancel it and finish the ride on the driver side as well
to cancel-ride ; patch method, called with this=ride-start patch implicitly
  let my-riders turtles with [current-ride-start = myself]

  ask turtles with [current-ride-start = myself] [
    driver-finish-ride
  ]

  close-ride self

  without-interruption [
    set num-clients-gave-up (num-clients-gave-up + 1)
  ]
end

;;;;; Driver procedures
to set-driver-color
  ifelse starting?
  [ set color yellow ]
  [ ifelse occupied? [set color red][set color green] ]
end

to put-on-empty-road ;; turtle procedure
  move-to one-of roads with [not any? turtles-on self]
end

to assign-rides-to-drivers
  without-interruption [
    let not-busy-drivers turtles with [not starting? and not occupied?]
    let not-taken-rides patches with [is-client? and not is-taken?]

    ask not-taken-rides [
      ; ensuring the same driver is not picked twice
      let not-picked-drivers not-busy-drivers with [not starting?]

      let assigned-driver min-one-of not-picked-drivers [distance myself]

      ; show "The assigned driver was"
      ; show assigned-driver

      start-ride assigned-driver self
    ]
  ]
end

to start-ride [driver ride-start]
  if is-turtle? driver and is-patch? ride-start [
    ; patch
    ask ride-start [
      set is-taken? true
    ]
    ; turtle
    ask driver [
      set starting? true
      set current-ride-start ride-start
      set current-path (a-star patch-here current-ride-start)
    ]
  ]
end

; For both finishing and cancelling rides, thus the seeming duplication
to driver-finish-ride ;; turtle procedure
  set starting? false
  set occupied? false
  set current-ride-start nobody
  set current-path []
  set current-ride-profit 0
end

; Switches to next ride state, if possible
; Calculates the path to the next target
to handle-ride-state ;; turtle procedure
  ifelse starting? [
    if patch-here = current-ride-start [
      ; the current ride will be started!
      set starting? false
      set occupied? true
      ; Calculate path from ride start to destination
      set current-path (a-star current-ride-start [my-ride-end] of current-ride-start)
      ; Store the path's length (to calculate profit when the ride is done)
      set current-ride-profit (calculate-profit (length current-path))
      ; mark the patch as tapped just to know it was reached
      set is-tapped? true
      set pcolor blue
      ; display ride end
      ask my-ride-end [
        set pcolor red
      ]
    ]
  ][
    if occupied? [
      if patch-here = [my-ride-end] of current-ride-start [
        ; increase company income based on ride profit
        without-interruption [
          set income (income + current-ride-profit)
        ]
        ; finish current ride
        close-ride current-ride-start
        driver-finish-ride
      ]
    ]
  ]
end

; Moves a driver one step in their path
to driver-move-one-tick ;; turtle procedure
  if not empty? current-path [
    let next-step (first current-path)
    ifelse next-step = patch-here [
      ; I'm already at the next step (usually happens with the first step)
      ; Just skip this patch and call the function again
      set current-path (but-first current-path)
      driver-move-one-tick
    ][
      face next-step
      ; smoothing movement and ensuring the turtle is not slightly off
      repeat 10 [fd 0.1]
      move-to next-step

      set current-path (but-first current-path)
    ]
  ]
end

to record-driver-data
  set num-drivers-occupied (count turtles with [occupied?])
  set num-drivers-starting (count turtles with [starting?])
end

to record-ride-data
  set num-rides (count patches with [is-client?])
  set num-rides-taken (count patches with [is-client? and is-taken?])
end

;;;;;;;;;;;;;;;;;;;;
;; Runtime
;;;;;;;;;;;;;;;;;;;;
to go
  if (simulation-max-ticks > 0 and ticks >= simulation-max-ticks) [
    stop
  ]

  if (next-arrival-time = ticks) [
    create-ride
  ]

  ask patches with [is-client? and not is-tapped? and ride-expire-tick != -1 and ticks >= ride-expire-tick] [
    cancel-ride
  ]

  assign-rides-to-drivers

  ask turtles [
    ; take-requests
    handle-ride-state
    driver-move-one-tick
    set-driver-color
  ]

  update-ride-patch-colors

  charge-expenses

  record-driver-data
  record-ride-data

  tick
end

;;;;; "Business logic"
to-report calculate-profit [path-length]
  report B + path-length * P
end

to charge-expenses
  set expenses (expenses + (driver-salary * (count turtles)))
end

;;;;; Ride color gradients
to update-ride-patch-colors
  ask patches with [is-client?] [
    ; [0,1[
    let progress-to-expire ((ticks - ride-created-tick) / (ride-expire-tick - ride-created-tick))
    ; [0,0.7[
    let normalized-progress (progress-to-expire * 0.7)

    let green-netlogo extract-rgb green
    let white-netlogo extract-rgb white

    if not is-tapped? [
      set pcolor palette:scale-gradient (list green-netlogo white-netlogo) normalized-progress 0 1
    ]
  ]
end

;;;;; Epicentre methods

to create-epicentres
  if mouse-down? [
    let x-mouse mouse-xcor
    let y-mouse mouse-ycor

    let selected-patch (patch x-mouse y-mouse)

    if (is-patch? selected-patch and (member? selected-patch roads)) [
      show "yes"
      show selected-patch
      set-epicentre selected-patch
    ]
  ]
end

to set-epicentre [selected-patch]
  ask selected-patch [set plabel "E"]
end

to clear-epicentres
  ask patches [ set plabel "" ]
end
@#$#@#$#@
GRAPHICS-WINDOW
393
10
921
539
-1
-1
13.0
1
10
1
1
1
0
0
0
1
0
39
-39
0
0
0
1
ticks
30.0

BUTTON
21
394
162
427
Setup with Grid
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
195
374
258
407
Go
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

PLOT
224
566
424
716
Drivers Mid-Ride
Time
NDrivers
0.0
100.0
0.0
100.0
true
false
"set-plot-y-range 0 num-drivers" ""
PENS
"default" 1.0 0 -16777216 true "" "plot num-drivers-occupied"

PLOT
19
566
219
716
Drivers Starting Ride
Time
NDrivers
0.0
100.0
0.0
100.0
true
false
"set-plot-y-range 0 num-drivers" ""
PENS
"default" 1.0 0 -16777216 true "" "plot num-drivers-starting"

PLOT
433
566
633
716
Active Rides
Time
NTrips
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"total" 1.0 0 -16777216 true "" "plot num-rides"
"taken" 1.0 0 -13840069 true "" "plot num-rides-taken"

SLIDER
18
19
212
52
num-drivers
num-drivers
1
20
1.0
1
1
NIL
HORIZONTAL

BUTTON
21
351
177
384
Setup with Image
setup-with-image user-file
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
650
566
850
716
Income
Time
Profit
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot income"

PLOT
856
566
1056
716
Expenses
Time
Expenses
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot expenses"

PLOT
1063
566
1263
716
Net Profit
Time
Profit
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (income - expenses)"

SLIDER
956
40
1244
73
mean-arrival-rate
mean-arrival-rate
1
20
7.0
1
1
every 20 ticks
HORIZONTAL

MONITOR
958
86
1083
131
Next Arrival Time
next-arrival-time
17
1
11

INPUTBOX
961
299
1133
359
ride-ticks-to-expire
50.0
1
0
Number

PLOT
1274
566
1474
716
Clients that Gave Up
Time
NClientsGaveUp
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot num-clients-gave-up"

BUTTON
960
199
1122
232
Create Epicentres
create-epicentres
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
961
240
1111
273
Clear Epicentres
clear-epicentres
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
960
159
1229
192
spawn-in-epicentre-chance
spawn-in-epicentre-chance
0
100
45.0
1
1
%
HORIZONTAL

INPUTBOX
18
67
179
127
ride-base-price
3.0
1
0
Number

INPUTBOX
18
135
179
195
price-per-ride-distance-unit
5.0
1
0
Number

INPUTBOX
18
204
179
264
salary-per-driver-per-tick
0.05
1
0
Number

INPUTBOX
963
402
1124
462
simulation-max-ticks
0.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

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
0
@#$#@#$#@
