globals [
  agr-comm-price
  urban-wage
        ]

patches-own [
  ;ag-suit     ;TO-DO: agricultural suitability (quality)
  state        ;either forest, crop, or fallow
            ]

breed [households household]
households-own [
  migrants       ;members earing an urban wage
  farmworkers    ;members working the line
  capital        ;money available for financing land clearings, migrant travels, houshold maintainance
  landholdings   ;composed of a certain number of patches on different states
               ]


;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

to setup
  ca
  set-default-shape turtles "house"
  create-landscape
  create-village
  create-social-network
  reset-ticks
end
;------------------------------------------------------------------------------
to create-landscape
  ;; MISSING: maybe include a higher probability for high ag-suit patches to start as crop(?)
  ask patches [
    ;create random landscape composed of patches in three posible states: forest, fallow land, and crop
    set pcolor one-of  [green brown yellow]
    ;match patch color with state
    if pcolor = green  [set state "forest"]
    if pcolor = brown  [set state "crop"]
    if pcolor = yellow [set state "forest"]
    ;; TO-DO: assign five levels of agricultural suitability to each parcel, randomly
       ;set ag-suit random 4
    ]
end
;------------------------------------------------------------------------------
to create-village
  create-households num-households [
    setxy random-xcor random-ycor
    set size 2
    set color gray
    ;; FROM: http://netlogo-users.18673.x6.nabble.com/Spacing-between-randomly-placed-turtles-td4861328.html
    move-to one-of patches with [not any? households in-radius 8]
    set capital ini-capital + random-float 1
    ; assign number of household members that are migrants and resident farmworkers (also defining houseold size)
    set migrants random 1
    set farmworkers random 5 + 2
    ; define household's landholdings, allowing for some variability in their size
    ;; TO-DO: draw a boundary around the land of each household to visualize this variability
    let land-size (random 3) + 1
    ;set forest-patches patches with [pcolor = green] in-radius (land-size)
    ;set crop-patches patches with [pcolor = yellow] in-radius (land-size)
    ;set fallow-patches patches with [pcolor = brown] in-radius (land-size)
    ;; AlTERNATIVELY: assign patches in random radius as landholdings, in which each household then makes decisions (rather than having separate accounts for each land cover/use type)
       set landholdings patches in-radius (land-size)
  ]
end

;------------------------------------------------------------------------------
to create-social-network
  ;; FROM 'Virus on a Network' in Model Library
  ; The network is created based on proximity (Euclidian distance) between households, until it matches a specified 'average node degree'
  ; TO-DO:
   let num-links (ave-household-degree * num-households) / 2
    while [count links < num-links] [
    ask one-of households [let choice (min-one-of (other households with [not link-neighbor? myself]) [distance myself])
    if choice != nobody [create-link-with choice]]]
   ask links [set color white]
end
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
to go
  labor-market
  agr-comm-market
  carry-on
  assess-decide
  sucession
  tick
  if ticks = 50
  [stop]
end
;------------------------------------------------------------------------------
to labor-market
  ; gradually increasing urban wages
  ifelse ticks = 0
   [set urban-wage ini-urban-wage ]
   [set urban-wage urban-wage + 0.5 ]
end
;------------------------------------------------------------------------------
to agr-comm-market
  ; gradually decreacing commodity prices
  ifelse ticks = 0
   [set agr-comm-price ini-agr-comm-price]
   [set agr-comm-price agr-comm-price - 0.1 ]
end
;------------------------------------------------------------------------------
to carry-on
  ask households [
  let crop-patches landholdings with [pcolor = yellow]
  let forest-patches landholdings with [pcolor = green]
  ifelse count crop-patches > 0
   [let earnings ((farmworkers / count crop-patches) * (count crop-patches) * agr-comm-price)
                + (urban-wage * migrants)
     ifelse ticks > 0
      [set capital capital + earnings - (farmworkers * 5)]
      [set capital ini-capital + earnings - (farmworkers * 5)]
   ]
   [if  any? forest-patches
     [ask one-of forest-patches [set pcolor yellow]
      ifelse ticks > 0
       [set capital capital + (urban-wage * migrants) - (farmworkers * 5)]
       [set capital ini-capital + (urban-wage * migrants) - (farmworkers * 5)]]
   ]
   ; if every household member migrates, then household no longer exists
   if farmworkers = 0
   [die]

   ;; TO-DO: consider including household life-cycle (births, deaths, aging, etc.)
  ]

end
;------------------------------------------------------------------------------
to assess-decide
  ; each houshold assess its assets (land, labor) and external conditions (prices and wages), to decide what to do next
  ask households [
    let crop-patches landholdings with [pcolor = yellow]
    let forest-patches landholdings with [pcolor = green]
    let fallow-patches landholdings with [pcolor = brown]
    let status-quo-uti (count crop-patches * farmworkers * agr-comm-price) + (urban-wage * migrants)

    let defo-cost (farmworkers * 0.01)
    let pot-extra-farm (((1 + count crop-patches) * farmworkers * agr-comm-price) - defo-cost)

    let migra-cost (10 / (1 + count (my-links)))
    let pot-extra-mig ((urban-wage * (1 + migrants)) - migra-cost)

    if pot-extra-farm > (status-quo-uti / 20)
     [if  any? forest-patches
       [ask one-of forest-patches [set pcolor yellow]]]
    ;
    if pot-extra-mig > (status-quo-uti / 20 )
     [if farmworkers > 0
       [set migrants migrants + 1
        set farmworkers farmworkers - 1]]
    ;
    if any? fallow-patches
     [if (count crop-patches) > (farmworkers * 2)
       [if random 9 = 1
         [ask one-of fallow-patches [set pcolor green]]]]

    if any? crop-patches
     [if (count crop-patches) > (farmworkers * 2)
       [if random 9 = 1
         [ask one-of crop-patches [set pcolor brown]]]]
]
end
;------------------------------------------------------------------------------
to sucession
  ask patches [
    if pcolor = brown
     [if random 9 = 1
       [set pcolor green]]]
end
@#$#@#$#@
GRAPHICS-WINDOW
197
12
914
750
50
50
7.0
1
10
1
1
1
0
0
0
1
-50
50
-50
50
0
0
1
ticks
30.0

BUTTON
12
230
78
263
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
122
230
185
263
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
8
38
180
71
num-households
num-households
0
100
100
1
1
NIL
HORIZONTAL

SLIDER
8
74
182
107
ave-household-degree
ave-household-degree
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
18
187
166
220
ini-urban-wage
ini-urban-wage
0
10
0
1
1
NIL
HORIZONTAL

SLIDER
17
149
165
182
ini-agr-comm-price
ini-agr-comm-price
0
10
6
1
1
NIL
HORIZONTAL

SLIDER
9
110
181
143
ini-capital
ini-capital
0
10
1
1
1
NIL
HORIZONTAL

PLOT
925
14
1347
158
demographics
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"migrants" 1.0 0 -7858858 true "" "plot (sum [migrants] of households)"
"farmworkers" 1.0 0 -4079321 true "" "plot (sum [farmworkers] of households)"
"household size" 1.0 0 -16777216 true "" "plot (sum [migrants + farmworkers] of households)"

PLOT
925
309
1348
467
landscape
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"forests" 1.0 0 -12087248 true "" "plot 100 * (count patches with [pcolor = green]) / count patches"
"cropland" 1.0 0 -4079321 true "" "plot 100 * (count patches with [pcolor = yellow]) / count patches"
"fallow lands" 1.0 0 -10402772 true "" "plot 100 * (count patches with [pcolor = brown]) / count patches"

PLOT
924
469
1347
637
social capital
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
"default" 1.0 1 -16777216 true "histogram [count (my-links)] of households" "histogram [count (my-links)] of households"

PLOT
925
163
1348
307
accounting
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"farm produce" 1.0 0 -7171555 true "" "if count households > 0\n[plot \n((sum [farmworkers] of turtles) / \n (sum [count landholdings with [pcolor = yellow]] of turtles)) * \n(sum [count landholdings with [pcolor = yellow]] of turtles) *\nagr-comm-price]"
"remittances" 1.0 0 -10022847 true "" "plot (urban-wage * \nsum [migrants] of households)"

BUTTON
124
267
188
301
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

@#$#@#$#@
## WHAT IS IT?

This model is a first attempt to describe a process through which rural households in a confined landscape, make decisions about the best way to distribute the members of the household to in-farm and off-farm activities. As each household decides, they alter their land parcels, and the landscape as a whole.

NOTE that this model is still requires quite some work. But it already includes the basics to represent this phenomenon.

AGENTS, their PROPERTIES and ACTIONS: There are two types of agents: ‘households’ (turtle-type agents) and ‘patches’ (patch-type agent).
Households assess the conditions, both external (market prices for the agricultural commodity produced and urban wages) and internal (landholdings, farmworking members and migrant members). Considering the potential payoffs, as well as the costs implied, each household decides on a marginal change of member labor distribution. Depending on their decisions, their household composition will change (number of farmworkers and migrants) as well as the composition of their landholdings (forest, fallow, or crop).
While some patches are part of the landhodings of a household, other patches do not. The former can be changed as described above. While the latter only change following a ecological sucession simple rule (i.e. abandoned crops eventually turn to forest)

ENVIRONMENT: The landscape is heterogeneous (three different land cover/use types) and dynamic (changing based on household decisions). Another aspect of the environment on which the agents operate is a social network that affects migration decisions. Finally, there are two signals from an external market, which are also part of the decision-making environment. These signals are the price of the single agricultural commodity considered and the wages for off-farm labor that migrants get.


## HOW IT WORKS

SETUP: creates the environment described above.

GO: Five procedures (so far):

labor-market:    gradual increase in urban wages (global).

agr-comm-market: gradual decrease in the price of the agricultural commodity produced                      (global).

carry-on:        is the basic household activity, which defines how their financial                        situation changes.

assess-decide:   actual decision of each household to change or maintain their economic                    activities, based on potential gains from further land conversion and                     emigration of members.
                 It also includes the change in their landholdings sucession: change of                    the remaining landscape which is not owned by any household, based on                     ecological sucession.

The simulation ends after 50 years (ticks), with the intention to span two generations (even though no additional demographic dynamics are simulated).


## HOW TO USE IT

The parameters that can be modified on the interface are:

num-households: simply the number of households in the landscape

ave-household-degree: the households conform a network, which can more or less dense depending on this average-degree

ini-capital: a base amount of financial capital each households starts with

ini-agr-com-price: initial price of the single agricultural commodity produced in this                       landscape (it will then progressively decrease)

ini-urban-wage: initial urban wage (it will then progressively increase)

The MONITORS on the interface are:

demographics: track the size of the household, amount of migrants, and amount of resident               farmworkers

accounting: tracks the income households obtain through farm produce and through                      remittances

landscape: tracks the amount of forest, crop, and fallow parcels on the landscape

social capital: shows the distribution of network links among households


## THINGS TO NOTICE

There is not much yet. The obvious, yet interesting thing is how croplands get abandoned and migration increases, and eventually the whole village is abandoned.

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
NetLogo 5.3.1
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
