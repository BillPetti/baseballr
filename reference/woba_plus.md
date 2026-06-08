# **Calculate wOBA and related metrics for any set of data**

This function allows you to calculate wOBA for any given set of data,
provided the right variables are in the data set. The function currently
returns both wOBA per plate appearance on wOBA per instance of fair
contact.

## Usage

``` r
woba_plus(df)
```

## Arguments

- df:

  A data frame of statistics that includes, at a minimum, the following
  columns: uBB (unintentional walks), HBP (Hit By Pitch), X1B (singles),
  X2B (doubles), X3B (triples), HR (home runs), AB (at-bats), SH
  (sacrifice hits), SO (strike outs), and season.

## Value

Returns a tibble with the wOBA factors calculated and the following
columns:

|          |           |
|----------|-----------|
| col_name | types     |
| bbref_id | character |
| season   | integer   |
| Name     | character |
| Age      | numeric   |
| Level    | character |
| Team     | character |
| G        | numeric   |
| PA       | numeric   |
| AB       | numeric   |
| R        | numeric   |
| H        | numeric   |
| X1B      | numeric   |
| X2B      | numeric   |
| X3B      | numeric   |
| HR       | numeric   |
| RBI      | numeric   |
| BB       | numeric   |
| IBB      | numeric   |
| uBB      | numeric   |
| SO       | numeric   |
| HBP      | numeric   |
| SH       | numeric   |
| SF       | numeric   |
| GDP      | numeric   |
| SB       | numeric   |
| CS       | numeric   |
| BA       | numeric   |
| OBP      | numeric   |
| SLG      | numeric   |
| OPS      | numeric   |
| wOBA     | numeric   |
| wOBA_CON | numeric   |

## Examples

``` r
# \donttest{
 try({
   df <- bref_daily_batter("2015-08-01", "2015-10-03") 
   woba_plus(df)
 })
#> ✖ 2026-06-08 01:58:40.771419: Invalid arguments or no daily batter data available!
#>     season                  Name Age         Level
#> 1     2015        Branden Pinder  26        Maj-AL
#> 2     2015        Pedro Severino  21        Maj-NL
#> 3     2015            Jett Bandy  25        Maj-AL
#> 4     2015         Daniel Norris  22        Maj-AL
#> 5     2015         Keith Hessler  26        Maj-NL
#> 6     2015             Rico Noel  26        Maj-AL
#> 7     2015         Carlos Torres  32        Maj-NL
#> 8     2015          Jason García  22        Maj-AL
#> 9     2015       Yovani Gallardo  29        Maj-AL
#> 10    2015             Joe Panik  24        Maj-NL
#> 11    2015       Slade Heathcott  24        Maj-AL
#> 12    2015         Kyle Kendrick  30        Maj-NL
#> 13    2015        Jarrett Parker  26        Maj-NL
#> 14    2015        Dilson Herrera  21        Maj-NL
#> 15    2015           Ryan Raburn  34        Maj-AL
#> 16    2015            Tim Hudson  39        Maj-NL
#> 17    2015     Edwin Encarnación  32        Maj-AL
#> 18    2015     Robinson Chirinos  31        Maj-AL
#> 19    2015         Peter O'Brien  24        Maj-NL
#> 20    2015    Franklin Gutiérrez  32        Maj-AL
#> 21    2015          Bryce Harper  22        Maj-NL
#> 22    2015       Jonathan Villar  24        Maj-AL
#> 23    2015           David Ortiz  39        Maj-AL
#> 24    2015            Max Stassi  24        Maj-AL
#> 25    2015          Zack Greinke  31        Maj-NL
#> 26    2015          Jason Rogers  27        Maj-NL
#> 27    2015            Joey Votto  31        Maj-NL
#> 28    2015        Bryan Anderson  28        Maj-AL
#> 29    2015          Jesse Chavez  31        Maj-AL
#> 30    2015      Justin De Fratus  27        Maj-NL
#> 31    2015         Scott Feldman  32        Maj-AL
#> 32    2015        Vidal Nuño III  27        Maj-AL
#> 33    2015     Travis Tartamella  27        Maj-NL
#> 34    2015           Chris Davis  29        Maj-AL
#> 35    2015          Cody Stanley  26        Maj-NL
#> 36    2015         José Bautista  34        Maj-AL
#> 37    2015         Shin-Soo Choo  32        Maj-AL
#> 38    2015       Matt den Dekker  27        Maj-NL
#> 39    2015          Michael Reed  22        Maj-NL
#> 40    2015        Ryan Zimmerman  30        Maj-NL
#> 41    2015         Mikie Mahtook  25        Maj-AL
#> 42    2015        Josh Donaldson  29        Maj-AL
#> 43    2015            Kyle Lohse  36        Maj-NL
#> 44    2015         Dalton Pompey  22        Maj-AL
#> 45    2015       Justin Ruggiano  33        Maj-NL
#> 46    2015       Chris Colabello  31        Maj-AL
#> 47    2015            Matt Duffy  26        Maj-AL
#> 48    2015          Mookie Betts  22        Maj-AL
#> 49    2015           Kris Bryant  23        Maj-NL
#> 50    2015     Jeremy Hellickson  28        Maj-NL
#> 51    2015       Kendrys Morales  32        Maj-AL
#> 52    2015         David Peralta  27        Maj-NL
#> 53    2015         Dustin Ackley  27        Maj-AL
#> 54    2015            Lucas Duda  29        Maj-NL
#> 55    2015         Rob Refsnyder  24        Maj-AL
#> 56    2015      Francisco Lindor  21        Maj-AL
#> 57    2015        Matt Carpenter  29        Maj-NL
#> 58    2015      Jackson Williams  29        Maj-NL
#> 59    2015            Ryan Braun  31        Maj-NL
#> 60    2015         Maikel Franco  22        Maj-NL
#> 61    2015         Josh A. Smith  27        Maj-NL
#> 62    2015        Starlin Castro  25        Maj-NL
#> 63    2015          Andre Ethier  33        Maj-NL
#> 64    2015 Jarrod Saltalamacchia  30        Maj-NL
#> 65    2015          Chris Carter  28        Maj-AL
#> 66    2015          Corey Seager  21        Maj-NL
#> 67    2015        Dustin Pedroia  31        Maj-AL
#> 68    2015      Christian Yelich  23        Maj-NL
#> 69    2015       Yoenis Céspedes  29        Maj-NL
#> 70    2015         Phil Gosselin  26        Maj-NL
#> 71    2015      Michael Brantley  28        Maj-AL
#> 72    2015       Christian Colón  26        Maj-AL
#> 73    2015      Andrew McCutchen  28        Maj-NL
#> 74    2015     Joey Terdoslavich  26        Maj-NL
#> 75    2015        Mike Moustakas  26        Maj-AL
#> 76    2015        J.P. Arencibia  29        Maj-AL
#> 77    2015         Nolan Arenado  24        Maj-NL
#> 78    2015    Jackie Bradley Jr.  25        Maj-AL
#> 79    2015     Enrique Hernández  23        Maj-NL
#> 80    2015            AJ Pollock  27        Maj-NL
#> 81    2015         Adrian Beltré  36        Maj-AL
#> 82    2015           Nelson Cruz  34        Maj-AL
#> 83    2015            Tommy Pham  27        Maj-NL
#> 84    2015           Miguel Sanó  22        Maj-AL
#> 85    2015          David Freese  32        Maj-AL
#> 86    2015         Brandon Guyer  29        Maj-AL
#> 87    2015        Sean Rodríguez  30        Maj-NL
#> 88    2015          Brandon Belt  27        Maj-NL
#> 89    2015           Khris Davis  27        Maj-NL
#> 90    2015            Tyson Ross  28        Maj-NL
#> 91    2015         Luis Valbuena  29        Maj-AL
#> 92    2015         Brett Wallace  28        Maj-NL
#> 93    2015         Robinson Canó  32        Maj-AL
#> 94    2015        Justin Morneau  34        Maj-NL
#> 95    2015         Andrés Blanco  31        Maj-NL
#> 96    2015           José Pirela  25        Maj-AL
#> 97    2015       Trayce Thompson  24        Maj-AL
#> 98    2015        Miguel Cabrera  32        Maj-AL
#> 99    2015      Desmond Jennings  28        Maj-AL
#> 100   2015            Mike Trout  23        Maj-AL
#> 101   2015        Danny Valencia  30        Maj-AL
#> 102   2015      Asdrúbal Cabrera  29        Maj-AL
#> 103   2015         Ramón Cabrera  25        Maj-NL
#> 104   2015     Curtis Granderson  34        Maj-NL
#> 105   2015           Jose Altuve  25        Maj-AL
#> 106   2015       Corey Dickerson  26        Maj-NL
#> 107   2015      Stephen Piscotty  24        Maj-NL
#> 108   2015       Eduardo Escobar  26        Maj-AL
#> 109   2015          Colby Rasmus  28        Maj-AL
#> 110   2015         Anthony Rizzo  25        Maj-NL
#> 111   2015             Greg Bird  22        Maj-AL
#> 112   2015           Ben Zobrist  34        Maj-AL
#> 113   2015        Carlos Beltrán  38        Maj-AL
#> 114   2015      Michael Conforto  22        Maj-NL
#> 115   2015            A.J. Ellis  34        Maj-NL
#> 116   2015        Sócrates Brito  22        Maj-NL
#> 117   2015            Adam Eaton  26        Maj-AL
#> 118   2015         Jason Heyward  25        Maj-NL
#> 119   2015          David Wright  32        Maj-NL
#> 120   2015         Yunel Escobar  32        Maj-NL
#> 121   2015      Paul Goldschmidt  27        Maj-NL
#> 122   2015         Daniel Murphy  30        Maj-NL
#> 123   2015            Tom Murphy  24        Maj-NL
#> 124   2015         Pedro Álvarez  28        Maj-NL
#> 125   2015           Jamie Romak  29        Maj-NL
#> 126   2015       Michael McKenry  30        Maj-NL
#> 127   2015      Cory Spangenberg  24        Maj-NL
#> 128   2015           Mark Trumbo  29        Maj-AL
#> 129   2015        Gordon Beckham  28        Maj-AL
#> 130   2015         Eric Campbell  28        Maj-NL
#> 131   2015           Jaff Decker  25        Maj-NL
#> 132   2015             John Jaso  31        Maj-AL
#> 133   2015         Manny Machado  22        Maj-AL
#> 134   2015         Marcus Semien  24        Maj-AL
#> 135   2015          Justin Upton  27        Maj-NL
#> 136   2015     Yangervis Solarte  27        Maj-NL
#> 137   2015         Blake Swihart  23        Maj-AL
#> 138   2015         J.D. Martinez  27        Maj-AL
#> 139   2015           Travis Shaw  25        Maj-AL
#> 140   2015         Chris Gimenez  32        Maj-AL
#> 141   2015     Francisco Liriano  31        Maj-NL
#> 142   2015       Xander Bogaerts  22        Maj-AL
#> 143   2015       Freddie Freeman  25        Maj-NL
#> 144   2015        Odúbel Herrera  23        Maj-NL
#> 145   2015             Matt Kemp  30        Maj-NL
#> 146   2015       Jonathan Lucroy  29        Maj-NL
#> 147   2015        Scott Schebler  24        Maj-NL
#> 148   2015      Steven Souza Jr.  26        Maj-AL
#> 149   2015            José Abreu  28        Maj-AL
#> 150   2015       Travis d'Arnaud  26        Maj-NL
#> 151   2015           Mike Napoli  33        Maj-AL
#> 152   2015         Jesús Aguilar  25        Maj-AL
#> 153   2015            Alec Asher  23        Maj-NL
#> 154   2015         Carlos Correa  20        Maj-AL
#> 155   2015           Adam Duvall  26        Maj-NL
#> 156   2015       Carlos González  29        Maj-NL
#> 157   2015           Brad Miller  25        Maj-AL
#> 158   2015        Randal Grichuk  23        Maj-NL
#> 159   2015            Mike Morse  33        Maj-NL
#> 160   2015           Rafael Ynoa  27        Maj-NL
#> 161   2015        Shawn O'Malley  27        Maj-AL
#> 162   2015            B.J. Upton  30        Maj-NL
#> 163   2015        Clint Robinson  30        Maj-NL
#> 164   2015         Dexter Fowler  29        Maj-NL
#> 165   2015         Bryan Holaday  27        Maj-AL
#> 166   2015        Kyle Schwarber  22        Maj-NL
#> 167   2015           Matt Szczur  25        Maj-NL
#> 168   2015         Chris Coghlan  30        Maj-NL
#> 169   2015         Eduardo Núñez  28        Maj-AL
#> 170   2015            Ryan Goins  27        Maj-AL
#> 171   2015          Martín Prado  31        Maj-NL
#> 172   2015           Kyle Seager  27        Maj-AL
#> 173   2015           Justin Bour  27        Maj-NL
#> 174   2015           Jorge Soler  23        Maj-NL
#> 175   2015       George Springer  25        Maj-AL
#> 176   2015         Aaron Altherr  24        Maj-NL
#> 177   2015          Billy Butler  29        Maj-AL
#> 178   2015          Nick Hundley  31        Maj-NL
#> 179   2015             C.J. Cron  25        Maj-AL
#> 180   2015        Logan Forsythe  28        Maj-AL
#> 181   2015          Jung Ho Kang  28        Maj-NL
#> 182   2015        Grady Sizemore  32        Maj-AL
#> 183   2015            Mark Canha  26        Maj-AL
#> 184   2015     Johnny Giavotella  27        Maj-AL
#> 185   2015          Juan Lagares  26        Maj-NL
#> 186   2015        Miguel Montero  31        Maj-NL
#> 187   2015         Marcell Ozuna  24        Maj-NL
#> 188   2015       Domingo Santana  22        Maj-NL
#> 189   2015        Ender Inciarte  24        Maj-NL
#> 190   2015             Adam Lind  31        Maj-NL
#> 191   2015           Ian Kinsler  33        Maj-AL
#> 192   2015          Buster Posey  28        Maj-NL
#> 193   2015       Kevin Kiermaier  25        Maj-AL
#> 194   2015      Nick Castellanos  23        Maj-AL
#> 195   2015           Evan Gattis  28        Maj-AL
#> 196   2015           Yasiel Puig  24        Maj-NL
#> 197   2015          José Ramírez  22        Maj-AL
#> 198   2015          Brandon Moss  31        Maj-NL
#> 199   2015      Brandon Phillips  34        Maj-NL
#> 200   2015       A.J. Pierzynski  38        Maj-NL
#> 201   2015         Wilmer Flores  23        Maj-NL
#> 202   2015       Marwin González  26        Maj-AL
#> 203   2015         Mark Reynolds  31        Maj-NL
#> 204   2015          Lorenzo Cain  29        Maj-AL
#> 205   2015         Carl Crawford  33        Maj-NL
#> 206   2015       Tommy La Stella  26        Maj-NL
#> 207   2015    Dee Strange-Gordon  27        Maj-NL
#> 208   2015         Tyler Flowers  29        Maj-AL
#> 209   2015           Eric Hosmer  25        Maj-AL
#> 210   2015        Howie Kendrick  31        Maj-NL
#> 211   2015       Abraham Almonte  26        Maj-AL
#> 212   2015      Charlie Blackmon  28        Maj-NL
#> 213   2015        Cole Gillespie  31        Maj-NL
#> 214   2015           Ian Desmond  29        Maj-NL
#> 215   2015           Ketel Marte  21        Maj-AL
#> 216   2015       Jonathan Schoop  23        Maj-AL
#> 217   2015    Francisco Cervelli  29        Maj-NL
#> 218   2015       Kelby Tomlinson  25        Maj-NL
#> 219   2015           Chase Utley  36        Maj-NL
#> 220   2015         Nick Markakis  31        Maj-NL
#> 221   2015      John Ryan Murphy  24        Maj-AL
#> 222   2015          Rougned Odor  21        Maj-AL
#> 223   2015          Steve Pearce  32        Maj-AL
#> 224   2015           Curt Casali  26        Maj-AL
#> 225   2015        Logan Morrison  27        Maj-AL
#> 226   2015        Dioner Navarro  31        Maj-AL
#> 227   2015          Jayson Werth  36        Maj-NL
#> 228   2015       Michael Cuddyer  36        Maj-NL
#> 229   2015           Rajai Davis  34        Maj-AL
#> 230   2015        Prince Fielder  31        Maj-AL
#> 231   2015        Mitch Moreland  29        Maj-AL
#> 232   2015         Paulo Orlando  29        Maj-AL
#> 233   2015        Aramis Ramírez  37        Maj-NL
#> 234   2015          Justin Smoak  28        Maj-AL
#> 235   2015           Neil Walker  29        Maj-NL
#> 236   2015         Darwin Barney  29        Maj-AL
#> 237   2015         Grégor Blanco  31        Maj-NL
#> 238   2015        Starling Marte  26        Maj-NL
#> 239   2015          Carlos Pérez  24        Maj-AL
#> 240   2015        Carlos Santana  29        Maj-AL
#> 241   2015         Roberto Pérez  26        Maj-AL
#> 242   2015          Josh Reddick  28        Maj-AL
#> 243   2015          Cameron Rupp  26        Maj-NL
#> 244   2015              JB Shuck  28        Maj-AL
#> 245   2015        Eugenio Suárez  23        Maj-NL
#> 246   2015          Rubén Tejada  25        Maj-NL
#> 247   2015        Alexei Ramírez  33        Maj-AL
#> 248   2015        Derek Dietrich  25        Maj-NL
#> 249   2015         Evan Longoria  29        Maj-AL
#> 250   2015             Wil Myers  24        Maj-NL
#> 251   2015             Darin Ruf  28        Maj-NL
#> 252   2015       Ronald Torreyes  22        Maj-NL
#> 253   2015            Cody Asche  25        Maj-NL
#> 254   2015         Josh Harrison  27        Maj-NL
#> 255   2015      Kirk Nieuwenhuis  27        Maj-NL
#> 256   2015            Ben Revere  27        Maj-AL
#> 257   2015        Chris Denorfia  34        Maj-NL
#> 258   2015        Austin Jackson  28 Maj-AL,Maj-NL
#> 259   2015          Kevin Pillar  26        Maj-AL
#> 260   2015          Stephen Vogt  30        Maj-AL
#> 261   2015           Jonny Gomes  34 Maj-AL,Maj-NL
#> 262   2015       Gregory Polanco  23        Maj-NL
#> 263   2015         Nolan Reimold  31        Maj-AL
#> 264   2015          Matt Wieters  29        Maj-AL
#> 265   2015    Adeiny Hechavarría  26        Maj-NL
#> 266   2015             Max Muncy  24        Maj-AL
#> 267   2015       Addison Russell  21        Maj-NL
#> 268   2015           Denard Span  31        Maj-NL
#> 269   2015           Javier Báez  22        Maj-NL
#> 270   2015           Billy Burns  25        Maj-AL
#> 271   2015     Lonnie Chisenhall  26        Maj-AL
#> 272   2015      Alejandro De Aza  31 Maj-AL,Maj-NL
#> 273   2015           Jedd Gyorko  26        Maj-NL
#> 274   2015         Kelly Johnson  33        Maj-NL
#> 275   2015           Jeff Mathis  32        Maj-NL
#> 276   2015          Derek Norris  26        Maj-NL
#> 277   2015        Anthony Rendon  25        Maj-NL
#> 278   2015            Alex Avila  28        Maj-AL
#> 279   2015         Adonis García  30        Maj-NL
#> 280   2015            Matt Adams  26        Maj-NL
#> 281   2015            Matt Hague  29        Maj-AL
#> 282   2015        Jake Marisnick  24        Maj-AL
#> 283   2015             Joe Mauer  32        Maj-AL
#> 284   2015        Richie Shaffer  24        Maj-AL
#> 285   2015         Elián Herrera  30        Maj-NL
#> 286   2015     Sugar Ray Marimón  26        Maj-NL
#> 287   2015          Brian McCann  31        Maj-AL
#> 288   2015       Clayton Richard  31        Maj-NL
#> 289   2015          Nick Swisher  34        Maj-NL
#> 290   2015      Pedro Villarreal  27        Maj-NL
#> 291   2015        Taijuan Walker  22        Maj-AL
#> 292   2015        Didi Gregorius  25        Maj-AL
#> 293   2015         Wilin Rosario  26        Maj-NL
#> 294   2015            Matt Duffy  24        Maj-NL
#> 295   2015           Eric Sogard  29        Maj-AL
#> 296   2015         Nick Franklin  24        Maj-AL
#> 297   2015         Josh Hamilton  34        Maj-AL
#> 298   2015           Ben Paulsen  27        Maj-NL
#> 299   2015     Madison Bumgarner  25        Maj-NL
#> 300   2015           Daniel Nava  32        Maj-AL
#> 301   2015         Yonder Alonso  28        Maj-NL
#> 302   2015           James Loney  31        Maj-AL
#> 303   2015         J.T. Realmuto  24        Maj-NL
#> 304   2015       Brian Bogusevic  31        Maj-NL
#> 305   2015         Justin Turner  30        Maj-NL
#> 306   2015          Anthony Gose  24        Maj-AL
#> 307   2015        Héctor Olivera  30        Maj-NL
#> 308   2015        Salvador Perez  25        Maj-AL
#> 309   2015            Juan Uribe  36        Maj-NL
#> 310   2015          Elvis Andrus  26        Maj-AL
#> 311   2015     Christian Bergman  27        Maj-NL
#> 312   2015       Steve Clevenger  29        Maj-AL
#> 313   2015           Ryan Lollis  28        Maj-NL
#> 314   2015          Hunter Pence  32        Maj-NL
#> 315   2015           Trea Turner  22        Maj-NL
#> 316   2015        Ryan Vogelsong  37        Maj-NL
#> 317   2015           Drew Butera  31        Maj-AL
#> 318   2015          Kole Calhoun  27        Maj-AL
#> 319   2015             Yan Gomes  27        Maj-AL
#> 320   2015      Martín Maldonado  28        Maj-NL
#> 321   2015         Tyler Collins  25        Maj-AL
#> 322   2015             Jake Lamb  24        Maj-NL
#> 323   2015           Brayan Peña  33        Maj-NL
#> 324   2015        Russell Martin  32        Maj-AL
#> 325   2015           Ángel Pagán  33        Maj-NL
#> 326   2015      Brandon Crawford  28        Maj-NL
#> 327   2015          Brett Lawrie  25        Maj-AL
#> 328   2015        Trevor Plouffe  29        Maj-AL
#> 329   2015         Henry Urrutia  28        Maj-AL
#> 330   2015         Melky Cabrera  30        Maj-AL
#> 331   2015            José Reyes  32        Maj-NL
#> 332   2015       Adrián González  33        Maj-NL
#> 333   2015         Chris Johnson  30 Maj-AL,Maj-NL
#> 334   2015         Eddie Rosario  23        Maj-AL
#> 335   2015        Skip Schumaker  35        Maj-NL
#> 336   2015      Ezequiel Carrera  28        Maj-AL
#> 337   2015          Carlos Gómez  29        Maj-AL
#> 338   2015          David Murphy  33        Maj-AL
#> 339   2015           Hank Conger  27        Maj-AL
#> 340   2015          Jarrod Dyson  30        Maj-AL
#> 341   2015         Jimmy Rollins  36        Maj-NL
#> 342   2015         Stefen Romero  26        Maj-AL
#> 343   2015       Jason Bourgeois  33        Maj-NL
#> 344   2015         Daniel Castro  22        Maj-NL
#> 345   2015       Scooter Gennett  25        Maj-NL
#> 346   2015          Ryan Hanigan  34        Maj-AL
#> 347   2015         Chase Headley  31        Maj-AL
#> 348   2015            Aaron Hill  33        Maj-NL
#> 349   2015            Brock Holt  27        Maj-AL
#> 350   2015       Rusney Castillo  27        Maj-AL
#> 351   2015           Alex Gordon  31        Maj-AL
#> 352   2015             Alex Ríos  34        Maj-AL
#> 353   2015        Alex Rodriguez  39        Maj-AL
#> 354   2015          Stephen Drew  32        Maj-AL
#> 355   2015        Dariel Álvarez  26        Maj-AL
#> 356   2015           Wilmer Difo  23        Maj-NL
#> 357   2015         Marco Estrada  31        Maj-AL
#> 358   2015        Jeremy Guthrie  36        Maj-AL
#> 359   2015           Cole Hamels  31        Maj-AL
#> 360   2015        Ubaldo Jiménez  31        Maj-AL
#> 361   2015            Adam Jones  29        Maj-AL
#> 362   2015             Mat Latos  27        Maj-NL
#> 363   2015           Adam Loewen  31        Maj-NL
#> 364   2015        Cameron Maybin  28        Maj-NL
#> 365   2015          Adam Rosales  32        Maj-AL
#> 366   2015         Josh Rutledge  26        Maj-AL
#> 367   2015        Avisaíl García  24        Maj-AL
#> 368   2015         Travis Snider  27 Maj-AL,Maj-NL
#> 369   2015           Aaron Hicks  25        Maj-AL
#> 370   2015           Jefry Marte  24        Maj-AL
#> 371   2015            Seth Smith  32        Maj-AL
#> 372   2015           Chris Young  31        Maj-AL
#> 373   2015    Welington Castillo  28        Maj-NL
#> 374   2015            Eury Pérez  25        Maj-NL
#> 375   2015         Albert Pujols  35        Maj-AL
#> 376   2015          Miguel Rojas  26        Maj-NL
#> 377   2015            Nick Ahmed  25        Maj-NL
#> 378   2015           Marlon Byrd  37        Maj-NL
#> 379   2015      Delino DeShields  22        Maj-AL
#> 380   2015       Troy Tulowitzki  30        Maj-AL
#> 381   2015             Tony Cruz  28        Maj-NL
#> 382   2015        Shane Robinson  30        Maj-AL
#> 383   2015           Kurt Suzuki  31        Maj-AL
#> 384   2015             Nori Aoki  33        Maj-NL
#> 385   2015          Caleb Joseph  29        Maj-AL
#> 386   2015            Juan Pérez  28        Maj-NL
#> 387   2015        Ehire Adrianza  25        Maj-NL
#> 388   2015          Craig Gentry  31        Maj-AL
#> 389   2015          Joc Pederson  23        Maj-NL
#> 390   2015         Logan Schafer  28        Maj-NL
#> 391   2015         Kennys Vargas  24        Maj-AL
#> 392   2015        Danny Espinosa  28        Maj-NL
#> 393   2015       César Hernández  25        Maj-NL
#> 394   2015           Ryan Howard  35        Maj-NL
#> 395   2015        Donovan Solano  27        Maj-NL
#> 396   2015           Chad Bettis  26        Maj-NL
#> 397   2015          Jason Kipnis  28        Maj-AL
#> 398   2015             Dan Uggla  35        Maj-NL
#> 399   2015          Josh Phegley  27        Maj-AL
#> 400   2015           DJ LeMahieu  26        Maj-NL
#> 401   2015             Colin Rea  24        Maj-NL
#> 402   2015     Andrelton Simmons  25        Maj-NL
#> 403   2015      Alberto Callaspo  32        Maj-NL
#> 404   2015        Jeff Francoeur  31        Maj-NL
#> 405   2015           Steven Moya  23        Maj-AL
#> 406   2015         Jace Peterson  25        Maj-NL
#> 407   2015          Todd Frazier  29        Maj-NL
#> 408   2015           Tom Koehler  29        Maj-NL
#> 409   2015          Jason Castro  28        Maj-AL
#> 410   2015         Ryan Flaherty  28        Maj-AL
#> 411   2015            Erik Kratz  35        Maj-NL
#> 412   2015        Jake Smolinski  26        Maj-AL
#> 413   2015           Greg Garcia  25        Maj-NL
#> 414   2015        Ryan Lavarnway  27        Maj-NL
#> 415   2015          Torii Hunter  39        Maj-AL
#> 416   2015           Paul Janish  32        Maj-AL
#> 417   2015         Dixon Machado  23        Maj-AL
#> 418   2015            Coco Crisp  35        Maj-AL
#> 419   2015         Jon Singleton  23        Maj-AL
#> 420   2015       Darnell Sweeney  24        Maj-NL
#> 421   2015       Tucker Barnhart  24        Maj-NL
#> 422   2015         Brandon Drury  22        Maj-NL
#> 423   2015            Joey Gallo  21        Maj-AL
#> 424   2015        Jhonny Peralta  33        Maj-NL
#> 425   2015          Bobby Wilson  32        Maj-AL
#> 426   2015          Jordy Mercer  28        Maj-NL
#> 427   2015           Mike Aviles  34        Maj-AL
#> 428   2015          Byron Buxton  21        Maj-AL
#> 429   2015       Travis Ishikawa  31        Maj-NL
#> 430   2015         Deven Marrero  24        Maj-AL
#> 431   2015            Jed Lowrie  31        Maj-AL
#> 432   2015              Mike Olt  26        Maj-AL
#> 433   2015        Shane Peterson  27        Maj-NL
#> 434   2015       Jacoby Ellsbury  31        Maj-AL
#> 435   2015         Freddy Galvis  25        Maj-NL
#> 436   2015      Noah Syndergaard  22        Maj-NL
#> 437   2015         Brett Gardner  31        Maj-AL
#> 438   2015          Chris Heisey  30        Maj-NL
#> 439   2015         Chris Stewart  33        Maj-NL
#> 440   2015       Scott Van Slyke  28        Maj-NL
#> 441   2015         Domonic Brown  27        Maj-NL
#> 442   2015          Brian Dozier  28        Maj-AL
#> 443   2015      Michael Martínez  32        Maj-AL
#> 444   2015        Yolmer Sanchez  23        Maj-AL
#> 445   2015           Tim Beckham  25        Maj-AL
#> 446   2015       Víctor Martínez  36        Maj-AL
#> 447   2015           Kolten Wong  24        Maj-NL
#> 448   2015      Cristhian Adames  23        Maj-NL
#> 449   2015               Jon Jay  30        Maj-NL
#> 450   2015         Gerardo Parra  28        Maj-AL
#> 451   2015           Erick Aybar  31        Maj-AL
#> 452   2015       Daniel Descalso  28        Maj-NL
#> 453   2015           Drew Stubbs  30 Maj-AL,Maj-NL
#> 454   2015         Austin Barnes  25        Maj-NL
#> 455   2015         Pedro Ciriaco  29        Maj-NL
#> 456   2015        David Holmberg  23        Maj-NL
#> 457   2015          Chris Owings  23        Maj-NL
#> 458   2015         Jose Iglesias  25        Maj-AL
#> 459   2015           Josh Wilson  34        Maj-AL
#> 460   2015         Iván De Jesús  28        Maj-NL
#> 461   2015        Billy Hamilton  24        Maj-NL
#> 462   2015     Michael A. Taylor  24        Maj-NL
#> 463   2015          Trevor Brown  23        Maj-NL
#> 464   2015           Jean Segura  25        Maj-NL
#> 465   2015          Will Venable  32 Maj-AL,Maj-NL
#> 466   2015          James McCann  25        Maj-AL
#> 467   2015        Pablo Sandoval  28        Maj-AL
#> 468   2015         Matt Holliday  35        Maj-NL
#> 469   2015         Yadier Molina  32        Maj-NL
#> 470   2015           Jerry Sands  27        Maj-AL
#> 471   2015      Cliff Pennington  31 Maj-AL,Maj-NL
#> 472   2015         Mark Teixeira  35        Maj-AL
#> 473   2015         Michael Bourn  32 Maj-AL,Maj-NL
#> 474   2015          Wilson Ramos  27        Maj-NL
#> 475   2015       Tyler Ladendorf  27        Maj-AL
#> 476   2015         Jesús Montero  25        Maj-AL
#> 477   2015            Eric Fryer  29        Maj-AL
#> 478   2015          Hernán Pérez  24        Maj-NL
#> 479   2015       Shane Victorino  34        Maj-AL
#> 480   2015          Jake Arrieta  29        Maj-NL
#> 481   2015          Max Scherzer  30        Maj-NL
#> 482   2015      Cheslor Cuthbert  22        Maj-AL
#> 483   2015        Chris Iannetta  32        Maj-AL
#> 484   2015           Tyler Moore  28        Maj-NL
#> 485   2015            Jake Peavy  34        Maj-NL
#> 486   2015      Travis Jankowski  24        Maj-NL
#> 487   2015           Gio Urshela  23        Maj-AL
#> 488   2015          Kaleb Cowart  23        Maj-AL
#> 489   2015            Pete Kozma  27        Maj-NL
#> 490   2015         Ichiro Suzuki  41        Maj-NL
#> 491   2015          Adam LaRoche  35        Maj-AL
#> 492   2015        Tyler Saladino  25        Maj-AL
#> 493   2015     Stephen Strasburg  26        Maj-NL
#> 494   2015             Jay Bruce  28        Maj-NL
#> 495   2015        Kevin Plawecki  24        Maj-NL
#> 496   2015       Taylor Jungmann  25        Maj-NL
#> 497   2015        Preston Tucker  24        Maj-AL
#> 498   2015         Yasmany Tomás  24        Maj-NL
#> 499   2015        Alexi Amarista  26        Maj-NL
#> 500   2015              Sam Fuld  33        Maj-AL
#> 501   2015        Brennan Boesch  30        Maj-NL
#> 502   2015         Alex Guerrero  28        Maj-NL
#> 503   2015         Andrew Romine  29        Maj-AL
#> 504   2015          Geovany Soto  32        Maj-AL
#> 505   2015 Christian Bethancourt  23        Maj-NL
#> 506   2015        Justin Maxwell  31        Maj-NL
#> 507   2015       Alcides Escobar  28        Maj-AL
#> 508   2015           José Peraza  21        Maj-NL
#> 509   2015          Mark Buehrle  36        Maj-AL
#> 510   2015         Trevor Cahill  27        Maj-NL
#> 511   2015           Kyle Parker  25        Maj-NL
#> 512   2015         Jonathan Diaz  30        Maj-AL
#> 513   2015    Taylor Featherston  25        Maj-AL
#> 514   2015       Óscar Hernández  21        Maj-NL
#> 515   2015        Chase d'Arnaud  28        Maj-NL
#> 516   2015     Munenori Kawasaki  34        Maj-AL
#> 517   2015         Jimmy Paredes  26        Maj-AL
#> 518   2015        Mac Williamson  24        Maj-NL
#> 519   2015             Ike Davis  28        Maj-AL
#> 520   2015          Leury García  24        Maj-AL
#> 521   2015          José Lobatón  30        Maj-NL
#> 522   2015           Carlos Ruiz  36        Maj-NL
#> 523   2015     Ryan Strausborger  27        Maj-AL
#> 524   2015           Adam Conley  25        Maj-NL
#> 525   2015        Collin Cowgill  29        Maj-AL
#> 526   2015        Alex Dickerson  25        Maj-NL
#> 527   2015           Buck Farmer  24        Maj-AL
#> 528   2015       Yasmani Grandal  26        Maj-NL
#> 529   2015            Adam Moore  31        Maj-AL
#> 530   2015            Sandy León  26        Maj-AL
#> 531   2015        Dustin Garneau  27        Maj-NL
#> 532   2015        Brandon Barnes  29        Maj-NL
#> 533   2015          Carson Blair  25        Maj-AL
#> 534   2015           René Rivera  31        Maj-AL
#> 535   2015           Jesús Sucre  27        Maj-AL
#> 536   2015             Jon Niese  28        Maj-NL
#> 537   2015          Omar Infante  33        Maj-AL
#> 538   2015          Jacob deGrom  27        Maj-NL
#> 539   2015      Jonathan Herrera  30        Maj-NL
#> 540   2015       Conor Gillaspie  27        Maj-AL
#> 541   2015           Grant Green  27        Maj-AL
#> 542   2015         Micah Johnson  24        Maj-AL
#> 543   2015        Kevin Frandsen  33        Maj-NL
#> 544   2015          Brendan Ryan  33        Maj-AL
#> 545   2015     Jordan Zimmermann  29        Maj-NL
#> 546   2015            J.J. Hardy  32        Maj-AL
#> 547   2015           Nick Noonan  26        Maj-NL
#> 548   2015             Alex Wood  24        Maj-NL
#> 549   2015        Pedro Florimón  28        Maj-NL
#> 550   2015         Austin Hedges  22        Maj-NL
#> 551   2015           Rob Brantly  25        Maj-AL
#> 552   2015        Héctor Sánchez  25        Maj-NL
#> 553   2015          Andrew Susac  25        Maj-NL
#> 554   2015        Hanley Ramírez  31        Maj-AL
#> 555   2015         Casey McGehee  32        Maj-NL
#> 556   2015          Yohan Flande  29        Maj-NL
#> 557   2015        Chris Herrmann  27        Maj-AL
#> 558   2015           Allen Craig  30        Maj-AL
#> 559   2015       Carlos Martínez  23        Maj-NL
#> 560   2015          Clint Barmes  36        Maj-NL
#> 561   2015        Patrick Corbin  25        Maj-NL
#> 562   2015            Jon Lester  31        Maj-NL
#> 563   2015         Terrance Gore  24        Maj-AL
#> 564   2015            David Ross  38        Maj-NL
#> 565   2015           James Jones  26        Maj-AL
#> 566   2015      Michael Lorenzen  23        Maj-NL
#> 567   2015           Doug Fister  31        Maj-NL
#> 568   2015        Sean Gilmartin  25        Maj-NL
#> 569   2015          Reed Johnson  38        Maj-NL
#> 570   2015        Anthony Recker  31        Maj-NL
#> 571   2015         Luis Sardiñas  22        Maj-NL
#> 572   2015         Peter Bourjos  28        Maj-NL
#> 573   2015           Joey Butler  29        Maj-AL
#> 574   2015       Josh Collmenter  29        Maj-NL
#> 575   2015           Junior Lake  25        Maj-AL
#> 576   2015         David DeJesus  35        Maj-AL
#> 577   2015           John Lackey  36        Maj-NL
#> 578   2015              Joe Ross  22        Maj-NL
#> 579   2015         Michael Wacha  23        Maj-NL
#> 580   2015           Mike Zunino  24        Maj-AL
#> 581   2015          Matt McBride  30        Maj-NL
#> 582   2015         Julio Teheran  24        Maj-NL
#> 583   2015         Danny Santana  24        Maj-AL
#> 584   2015             Dan Haren  34        Maj-NL
#> 585   2015           Chris Rusin  28        Maj-NL
#> 586   2015      Christian Walker  24        Maj-AL
#> 587   2015            Tyler Holt  26 Maj-AL,Maj-NL
#> 588   2015        José Fernández  22        Maj-NL
#> 589   2015         Leonys Martín  27        Maj-AL
#> 590   2015         Efren Navarro  29        Maj-AL
#> 591   2015          Tanner Roark  28        Maj-NL
#> 592   2015         Bartolo Colón  42        Maj-NL
#> 593   2015         Shelby Miller  24        Maj-NL
#> 594   2015           Tomás Telis  24        Maj-NL
#> 595   2015            Luke Maile  24        Maj-AL
#> 596   2015        Hanser Alberto  22        Maj-AL
#> 597   2015          Nevin Ashley  30        Maj-NL
#> 598   2015           Gerrit Cole  24        Maj-NL
#> 599   2015          Jason Hammel  32        Maj-NL
#> 600   2015       Raisel Iglesias  25        Maj-NL
#> 601   2015        Jerad Eickhoff  24        Maj-NL
#> 602   2015           Zack Godley  25        Maj-NL
#> 603   2015          Jimmy Nelson  26        Maj-NL
#> 604   2015          Gio Gonzalez  29        Maj-NL
#> 605   2015            Mike Leake  27        Maj-NL
#> 606   2015            Josh Thole  28        Maj-AL
#> 607   2015           Matt Wisler  22        Maj-NL
#> 608   2015            Matt Joyce  30        Maj-AL
#> 609   2015       Masahiro Tanaka  26        Maj-AL
#> 610   2015         James Shields  33        Maj-NL
#> 611   2015          Jaime García  28        Maj-NL
#> 612   2015        Jhoulys Chacín  27        Maj-NL
#> 613   2015           Steven Matz  24        Maj-NL
#> 614   2015       Clayton Kershaw  27        Maj-NL
#> 615   2015    Anthony DeSclafani  25        Maj-NL
#> 616   2015              Jon Gray  23        Maj-NL
#> 617   2015          Wily Peralta  26        Maj-NL
#> 618   2015        Brett Anderson  27        Maj-NL
#> 619   2015            Rocky Gale  27        Maj-NL
#> 620   2015          Aaron Harang  37        Maj-NL
#> 621   2015           Tyler Lyons  27        Maj-NL
#> 622   2015              Ryan Rua  25        Maj-AL
#> 623   2015      Rubby De La Rosa  26        Maj-NL
#> 624   2015            John Hicks  25        Maj-AL
#> 625   2015             Brad Hand  25        Maj-NL
#> 626   2015          Chris Heston  27        Maj-NL
#> 627   2015           David Lough  29        Maj-AL
#> 628   2015        Eric Young Jr.  30        Maj-NL
#> 629   2015             John Lamb  24        Maj-NL
#> 630   2015       Todd Cunningham  26        Maj-NL
#> 631   2015          Ryan LaMarre  26        Maj-NL
#> 632   2015          Ryan Jackson  27        Maj-AL
#> 633   2015          Jason Pridie  31        Maj-AL
#> 634   2015           Zach Davies  22        Maj-NL
#> 635   2015         Yadiel Rivera  23        Maj-NL
#> 636   2015           Adam Morgan  25        Maj-NL
#> 637   2015        Chase Anderson  27        Maj-NL
#> 638   2015      Jorge De La Rosa  34        Maj-NL
#> 639   2015            Lance Lynn  28        Maj-NL
#> 640   2015        Williams Pérez  24        Maj-NL
#> 641   2015            Robbie Ray  23        Maj-NL
#> 642   2015             J.A. Happ  32        Maj-NL
#> 643   2015        Andrew Cashner  28        Maj-NL
#> 644   2015       Justin Nicolino  23        Maj-NL
#> 645   2015        Jeremy Affeldt  36        Maj-NL
#> 646   2015           Luis Avilán  25        Maj-NL
#> 647   2015         Dylan Axelrod  29        Maj-NL
#> 648   2015        Burke Badenhop  32        Maj-NL
#> 649   2015       Collin Balester  29        Maj-NL
#> 650   2015        Manny Banuelos  24        Maj-NL
#> 651   2015          Steven Baron  24        Maj-AL
#> 652   2015      Antonio Bastardo  29        Maj-NL
#> 653   2015         Quintin Berry  30        Maj-NL
#> 654   2015           Joe Blanton  34        Maj-NL
#> 655   2015        Michael Blazek  26        Maj-NL
#> 656   2015        Mike Bolsinger  27        Maj-NL
#> 657   2015      Emilio Bonifácio  30        Maj-AL
#> 658   2015          Jake Brigham  27        Maj-NL
#> 659   2015          Aaron Brooks  25        Maj-AL
#> 660   2015          Keon Broxton  25        Maj-NL
#> 661   2015        David Buchanan  26        Maj-NL
#> 662   2015          A.J. Burnett  38        Maj-NL
#> 663   2015          Eddie Butler  24        Maj-NL
#> 664   2015             Matt Cain  30        Maj-NL
#> 665   2015        Garin Cecchini  24        Maj-AL
#> 666   2015       Aroldis Chapman  27        Maj-NL
#> 667   2015        Tyler Clippard  30        Maj-NL
#> 668   2015         Jarred Cosart  25        Maj-NL
#> 669   2015           Tyler Cravy  25        Maj-NL
#> 670   2015          Jordan Danks  28        Maj-NL
#> 671   2015           Cody Decker  28        Maj-NL
#> 672   2015   Odrisamer Despaigne  28        Maj-NL
#> 673   2015         Ross Detwiler  29        Maj-NL
#> 674   2015            Elias Díaz  24        Maj-NL
#> 675   2015           R.A. Dickey  40        Maj-AL
#> 676   2015      Carl Edwards Jr.  23        Maj-NL
#> 677   2015        Nathan Eovaldi  25        Maj-AL
#> 678   2015          Robbie Erlin  24        Maj-NL
#> 679   2015      Brandon Finnegan  22        Maj-NL
#> 680   2015         Kendry Flores  23        Maj-NL
#> 681   2015      Mike Foltynewicz  23        Maj-NL
#> 682   2015          Carlos Frías  25        Maj-NL
#> 683   2015            Matt Garza  31        Maj-NL
#> 684   2015          Erik Goeddel  26        Maj-NL
#> 685   2015         David Goforth  26        Maj-NL
#> 686   2015          Héctor Gómez  27        Maj-NL
#> 687   2015         Jeanmar Gómez  27        Maj-NL
#> 688   2015            Sonny Gray  25        Maj-AL
#> 689   2015            David Hale  27        Maj-NL
#> 690   2015          Blaine Hardy  28        Maj-AL
#> 691   2015           Matt Harvey  26        Maj-NL
#> 692   2015         Chris Hatcher  30        Maj-NL
#> 693   2015         Andrew Heaney  24        Maj-AL
#> 694   2015        Kyle Hendricks  25        Maj-NL
#> 695   2015       Félix Hernández  29        Maj-AL
#> 696   2015         Edwin Jackson  31        Maj-NL
#> 697   2015          Scott Kazmir  31        Maj-AL
#> 698   2015           Casey Kelly  25        Maj-NL
#> 699   2015             Joe Kelly  27        Maj-AL
#> 700   2015           Ian Kennedy  30        Maj-NL
#> 701   2015            Max Kepler  22        Maj-AL
#> 702   2015        Dallas Keuchel  27        Maj-AL
#> 703   2015         Craig Kimbrel  27        Maj-NL
#> 704   2015          Corey Kluber  29        Maj-AL
#> 705   2015         George Kontos  30        Maj-NL
#> 706   2015          Kyle Kubitza  24        Maj-AL
#> 707   2015            Sam LeCure  31        Maj-NL
#> 708   2015           Colby Lewis  35        Maj-AL
#> 709   2015            Jeff Locke  27        Maj-NL
#> 710   2015           Jorge López  22        Maj-NL
#> 711   2015           Seth Maness  26        Maj-NL
#> 712   2015         Collin McHugh  28        Maj-AL
#> 713   2015      Andrew McKirahan  25        Maj-NL
#> 714   2015            Wade Miley  28        Maj-AL
#> 715   2015        Bryan Mitchell  24        Maj-AL
#> 716   2015         Johnny Monell  29        Maj-NL
#> 717   2015        Charlie Morton  31        Maj-NL
#> 718   2015        Chris Narveson  33        Maj-NL
#> 719   2015     Kristopher Negrón  29        Maj-NL
#> 720   2015          Hector Neris  26        Maj-NL
#> 721   2015            Aaron Nola  22        Maj-NL
#> 722   2015           Henry Owens  22        Maj-AL
#> 723   2015            Ariel Peña  26        Maj-NL
#> 724   2015        Francisco Peña  25        Maj-AL
#> 725   2015          David Phelps  28        Maj-NL
#> 726   2015        Michael Pineda  26        Maj-AL
#> 727   2015           David Price  29        Maj-AL
#> 728   2015           Cory Rasmus  27        Maj-AL
#> 729   2015          Addison Reed  26        Maj-NL
#> 730   2015          André Rienzo  26        Maj-NL
#> 731   2015      Daniel Robertson  29        Maj-AL
#> 732   2015     Eduardo Rodríguez  22        Maj-AL
#> 733   2015         Austin Romine  26        Maj-AL
#> 734   2015         James Russell  29        Maj-NL
#> 735   2015           CC Sabathia  34        Maj-AL
#> 736   2015       Keyvius Sampson  24        Maj-NL
#> 737   2015        Aníbal Sánchez  31        Maj-AL
#> 738   2015          Gary Sánchez  22        Maj-AL
#> 739   2015         Luis Severino  21        Maj-AL
#> 740   2015            Ian Thomas  28        Maj-NL
#> 741   2015       Tyler Thornburg  26        Maj-NL
#> 742   2015         Chris Tillman  27        Maj-AL
#> 743   2015         Blake Treinen  27        Maj-NL
#> 744   2015            José Ureña  23        Maj-NL
#> 745   2015          Pat Venditte  30        Maj-AL
#> 746   2015       Yordano Ventura  24        Maj-AL
#> 747   2015         Logan Verrett  25        Maj-NL
#> 748   2015     Carlos Villanueva  31        Maj-NL
#> 749   2015       Edinson Vólquez  31        Maj-AL
#> 750   2015          Tyler Wagner  24        Maj-NL
#> 751   2015          Kyle Waldrop  23        Maj-NL
#> 752   2015          Zach Walters  25        Maj-AL
#> 753   2015            Ryan Weber  24        Maj-NL
#> 754   2015         Allen Webster  25        Maj-NL
#> 755   2015       Jerome Williams  33        Maj-NL
#> 756   2015          Tyler Wilson  25        Maj-AL
#> 757   2015           Travis Wood  28        Maj-NL
#> 758   2015         Steven Wright  30        Maj-AL
#> 759   2015          Erik Cordier  29        Maj-NL
#> 760   2015       Randall Delgado  25        Maj-NL
#> 761   2015           Kris Medlen  29        Maj-AL
#> 762   2015         Justin Miller  28        Maj-NL
#> 763   2015          Juan Nicasio  28        Maj-NL
#> 764   2015            Josh Osich  26        Maj-NL
#>                         Team  G  PA  AB  R  H X1B X2B X3B HR RBI BB IBB
#> 1                   New York  1   1   1  0  1   0   1   0  0   1  0   0
#> 2                 Washington  1   1   1  1  1   0   1   0  0   0  0   0
#> 3                Los Angeles  2   2   2  1  1   0   0   0  1   1  0   0
#> 4                    Detroit  1   3   2  1  1   0   0   0  1   2  1   0
#> 5                    Arizona  1   1   1  0  1   1   0   0  0   0  0   0
#> 6                   New York 13   1   1  5  1   1   0   0  0   0  0   0
#> 7                   New York  1   1   1  1  1   1   0   0  0   0  0   0
#> 8                  Baltimore  1   1   0  0  0   0   0   0  0   0  1   0
#> 9                      Texas  1   2   2  0  1   0   1   0  0   0  0   0
#> 10             San Francisco  4  14  12  5  5   0   4   0  1   3  1   0
#> 11                  New York  7  12   8  3  4   2   1   0  1   5  2   0
#> 12                  Colorado  5   9   7  3  3   2   0   0  1   1  2   0
#> 13             San Francisco 16  41  36 10 14   6   2   0  6  14  5   0
#> 14                  New York  5  10   8  2  3   1   1   0  1   2  2   0
#> 15                 Cleveland 18  49  40  9 16   9   3   0  4   7  7   0
#> 16             San Francisco  3   7   6  1  2   1   0   0  1   1  0   0
#> 17                   Toronto 49 216 180 43 63  29  14   0 20  55 27   4
#> 18                     Texas  5  15  12  5  4   2   1   0  1   1  2   0
#> 19                   Arizona  7  11   9  1  3   2   0   0  1   3  2   0
#> 20                   Seattle 35 120 108 22 34  16   6   0 12  27  9   1
#> 21                Washington 57 248 193 50 64  38  13   0 13  31 50   4
#> 22                   Houston 14  23  21  6  8   5   2   0  1   4  2   0
#> 23                    Boston 51 213 179 32 58  23  18   0 17  49 28   7
#> 24                   Houston  9  17  15  4  6   5   0   0  1   2  1   0
#> 25               Los Angeles  9  23  20  5  7   4   1   0  2   2  0   0
#> 26                 Milwaukee 35  74  63 12 24  18   3   1  2   9 10   0
#> 27                Cincinnati 57 251 183 37 58  35  13   0 10  29 65   9
#> 28                   Oakland  2   5   4  0  2   2   0   0  0   0  0   0
#> 29                   Oakland  1   2   2  0  1   1   0   0  0   0  0   0
#> 30              Philadelphia  2   2   2  0  1   1   0   0  0   0  0   0
#> 31                   Houston  1   2   2  0  1   1   0   0  0   0  0   0
#> 32                   Seattle  1   2   2  0  1   1   0   0  0   1  0   0
#> 33                 St. Louis  2   2   2  0  1   1   0   0  0   0  0   0
#> 34                 Baltimore 59 253 208 44 58  25  13   0 20  43 39   4
#> 35                 St. Louis  6   7   7  1  3   2   1   0  0   3  0   0
#> 36                   Toronto 55 245 205 45 58  27  11   1 19  46 38   0
#> 37                     Texas 58 260 211 48 71  47  14   1  9  34 39   1
#> 38                Washington 26  54  49  7 16   7   6   0  3   7  4   0
#> 39                 Milwaukee  4   5   5  2  2   1   1   0  0   0  0   0
#> 40                Washington 35 139 120 20 38  17  11   0 10  38 14   0
#> 41                 Tampa Bay 26  82  76 15 24  13   4   1  6  13  4   0
#> 42                   Toronto 55 255 213 50 65  34  13   2 16  50 34   0
#> 43                 Milwaukee  3   3   3  0  1   0   1   0  0   0  0   0
#> 44                   Toronto 10  11  10  6  4   3   1   0  0   0  1   0
#> 45               Los Angeles 19  59  54 12 16   7   4   1  4  12  3   0
#> 46                   Toronto 35 103  95 15 32  20   6   1  5  14  6   0
#> 47                   Houston  7   9   8  0  3   2   1   0  0   3  1   0
#> 48                    Boston 47 223 204 40 71  44  17   2  8  29 16   0
#> 49                   Chicago 57 241 216 32 69  41  15   1 12  39 21   0
#> 50                   Arizona  6  13  10  1  4   4   0   0  0   4  2   0
#> 51               Kansas City 55 223 191 35 59  34  13   2 10  34 29   2
#> 52                   Arizona 55 197 179 21 64  46   8   2  8  30 15   1
#> 53                  New York 21  51  46  5 13   5   3   1  4   9  4   0
#> 54                  New York 34 134 110 17 29  11   9   0  9  28 22   4
#> 55                  New York 11  34  31  2 11   7   3   0  1   3  3   1
#> 56                 Cleveland 58 259 224 35 79  51  17   4  7  32 18   0
#> 57                 St. Louis 56 245 216 42 61  26  18   2 15  32 26   1
#> 58             San Francisco  4  10   6  1  1   0   1   0  0   0  4   0
#> 59                 Milwaukee 43 173 152 29 50  33  10   1  6  21 19   1
#> 60              Philadelphia 12  46  40  7 11   3   5   0  3  12  4   1
#> 61                Cincinnati  4   4   4  1  1   0   0   1  0   0  0   0
#> 62                   Chicago 51 160 151 20 52  31  14   1  6  25  5   2
#> 63               Los Angeles 48 139 127 17 42  25  10   3  4  19 11   0
#> 64                   Arizona 47 113 101 16 29  14   8   0  7  18 10   0
#> 65                   Houston 35  97  82 13 21   8   6   0  7  19 12   0
#> 66               Los Angeles 26 109  95 16 30  18   8   1  3  16 13   1
#> 67                    Boston 17  84  75 11 24  16   4   1  3   8  9   0
#> 68                     Miami 42 180 166 28 60  39  19   1  1  19 14   2
#> 69                  New York 55 246 227 39 65  31  13   4 17  44 14   3
#> 70                   Arizona 23  71  61 17 18  10   4   1  3  13  7   0
#> 71                 Cleveland 44 188 172 27 57  35  15   0  7  28 14   3
#> 72               Kansas City 13  31  28  3 11  10   1   0  0   2  3   0
#> 73                Pittsburgh 57 252 198 34 58  38  11   1  8  30 47   7
#> 74                   Atlanta  9  24  22  3  7   3   3   1  0   3  2   0
#> 75               Kansas City 52 213 190 27 54  25  17   0 12  43 18   1
#> 76                 Tampa Bay 24  73  71  9 22  13   3   0  6  17  1   0
#> 77                  Colorado 58 251 233 40 71  37  17   0 17  52 13   3
#> 78                    Boston 54 205 177 40 48  19  16   4  9  40 23   0
#> 79               Los Angeles 26  91  85  7 29  22   3   0  4  10  5   0
#> 80                   Arizona 55 241 217 42 71  44  16   3  8  30 18   0
#> 81                     Texas 59 257 231 37 75  48  16   2  9  53 21   1
#> 82                   Seattle 50 222 198 37 55  32   6   0 17  30 21   4
#> 83                 St. Louis 36 119 106 23 32  20   3   5  4  15 12   0
#> 84                 Minnesota 57 238 201 34 52  26  11   1 14  38 35   1
#> 85               Los Angeles 30 113 103 13 33  20  10   0  3  13  6   0
#> 86                 Tampa Bay 46 146 128 26 38  21  12   1  4   7  7   0
#> 87                Pittsburgh 42  78  71 13 24  15   8   0  1  11  2   0
#> 88             San Francisco 43 182 160 28 47  30   7   2  8  25 19   0
#> 89                 Milwaukee 57 219 197 35 51  27   4   0 20  44 20   1
#> 90                 San Diego 11  27  21  3  8   7   1   0  0   4  2   0
#> 91                   Houston 43 129 110 15 31  18   7   0  6  16 17   1
#> 92                 San Diego 40  74  66 12 19  11   4   0  4  11  7   1
#> 93                   Seattle 57 248 228 32 75  55  10   0 10  37 19   2
#> 94                  Colorado 21  77  67  8 22  16   3   3  0   4 10   1
#> 95              Philadelphia 47 154 139 24 42  25  10   2  5  13 12   0
#> 96                  New York  7   7   7  1  3   3   0   0  0   1  0   0
#> 97                   Chicago 42 131 119 17 35  20   8   2  5  16 12   0
#> 98                   Detroit 42 178 152 21 48  33  12   0  3  22 24   2
#> 99                 Tampa Bay 10  36  34  2 12   9   1   1  1   5  1   0
#> 100              Los Angeles 59 253 207 27 55  30  12   4  9  23 42   3
#> 101                  Oakland 46 202 180 33 51  29  10   1 11  37 20   3
#> 102                Tampa Bay 53 212 191 31 61  41  10   1  9  34 15   1
#> 103               Cincinnati 13  30  30  4 11   9   1   0  1   3  0   0
#> 104                 New York 53 242 198 44 51  28  13   1  9  33 38   0
#> 105                  Houston 57 262 244 30 81  53  19   3  6  18 10   1
#> 106                 Colorado 21  77  74 13 21   8   9   0  4  10  3   0
#> 107                St. Louis 52 219 200 25 61  38  12   4  7  35 17   2
#> 108                Minnesota 53 202 182 31 52  25  18   1  8  27 18   1
#> 109                  Houston 50 186 163 31 41  22   6   1 12  25 21   0
#> 110                  Chicago 58 251 213 38 56  31  12   1 12  42 27   3
#> 111                 New York 44 174 153 25 39  20   8   0 11  30 19   0
#> 112              Kansas City 56 251 220 37 65  41  16   1  7  20 29   1
#> 113                 New York 56 226 199 31 57  34  12   0 11  34 23   1
#> 114                 New York 48 168 152 25 42  21  12   0  9  23 14   0
#> 115              Los Angeles 29 109  91 15 23  13   5   0  5  14 16   0
#> 116                  Arizona 17  29  29  5 10   6   3   1  0   1  0   0
#> 117                  Chicago 58 262 230 37 74  56  12   1  5  31 23   1
#> 118                St. Louis 54 220 192 31 58  40  12   2  4  25 27   0
#> 119                 New York 29 135 116 21 33  22   7   0  4  13 19   0
#> 120               Washington 48 213 186 28 59  47   8   0  4  25 24   0
#> 121                  Arizona 56 241 203 34 54  29  14   1 10  31 34   6
#> 122                 New York 49 208 195 33 59  30  19   2  8  37  9   0
#> 123                 Colorado 11  39  35  5  9   5   1   0  3   9  4   1
#> 124               Pittsburgh 55 155 135 18 34  20   3   0 11  25 19   2
#> 125                  Arizona 12  16  15  2  5   3   2   0  0   1  1   0
#> 126                 Colorado  9  24  18  2  3   0   2   0  1   5  5   0
#> 127                San Diego 41 145 126 17 37  23   9   3  2  10 14   0
#> 128                  Seattle 50 191 172 26 49  33   6   0 10  27 18   1
#> 129                  Chicago 21  44  37  7 11   9   0   0  2   4  4   1
#> 130                 New York 11  23  21  6  7   5   2   0  0   1  1   0
#> 131               Pittsburgh 13  17  15  3  5   4   1   0  0   1  2   0
#> 132                Tampa Bay 49 153 130 18 36  21  11   0  4  15 21   1
#> 133                Baltimore 59 266 237 36 66  43  10   0 13  32 26   1
#> 134                  Oakland 51 195 176 27 50  33   6   4  7  23 18   0
#> 135                San Diego 52 219 188 34 48  22  16   2  8  25 28   2
#> 136                San Diego 57 231 210 33 62  39  12   2  9  28 13   0
#> 137                   Boston 36 140 128 26 40  29   7   0  4  17 10   0
#> 138                  Detroit 56 229 207 26 57  31  15   1 10  33 20   1
#> 139                   Boston 55 228 207 29 57  34  10   0 13  36 17   1
#> 140                    Texas 31 106  93 18 24  12   6   1  5  13  9   0
#> 141               Pittsburgh 10  25  25  4  8   5   2   0  1   5  0   0
#> 142                   Boston 55 251 232 35 77  60  13   0  4  33 16   1
#> 143                  Atlanta 43 169 134 17 34  23   7   0  4  21 29   1
#> 144             Philadelphia 55 224 202 28 65  53   8   0  4  15 17   0
#> 145                San Diego 52 221 203 31 58  36   9   1 12  44 14   0
#> 146                Milwaukee 39 143 125 22 36  22   8   2  4  20 15   0
#> 147              Los Angeles 16  34  30  6  7   4   0   0  3   4  3   1
#> 148                Tampa Bay 21  77  63 12 18  13   4   0  1   6 11   0
#> 149                  Chicago 58 258 235 29 66  37  17   0 12  40 17   6
#> 150                 New York 46 183 162 20 43  24  11   0  8  24 18   0
#> 151             Boston,Texas 38 112  97 11 26  17   4   0  5  11 13   2
#> 152                Cleveland  3   6   6  0  2   1   1   0  0   0  0   0
#> 153             Philadelphia  4   6   6  0  2   1   1   0  0   0  0   0
#> 154                  Houston 53 232 204 26 54  33   8   1 12  39 26   2
#> 155               Cincinnati 25  68  60  6 14   7   2   0  5   9  6   1
#> 156                 Colorado 57 228 210 35 53  27   7   0 19  45 14   3
#> 157                  Seattle 46 149 133 16 41  29   8   1  3  13 12   0
#> 158                St. Louis 30  90  84 13 21  10   4   1  6  11  6   0
#> 159               Pittsburgh 44  81  68  6 19  14   3   1  1   7 11   0
#> 160                 Colorado 17  36  36  3 12   7   4   1  0   4  0   0
#> 161                  Seattle 24  57  42 10 11   9   1   0  1   7 12   0
#> 162                San Diego 49 130 117 11 33  18  10   2  3  11 12   1
#> 163               Washington 48 140 120 14 33  24   3   0  6  16 16   3
#> 164                  Chicago 57 255 218 37 56  32  13   4  7  19 33   1
#> 165                  Detroit  7  13  13  1  3   0   2   0  1   1  0   0
#> 166                  Chicago 48 199 167 40 36  19   4   0 13  31 27   0
#> 167                  Chicago 14  18  15  0  4   2   2   0  0   0  3   0
#> 168                  Chicago 48 164 143 27 36  19   7   5  5  17 19   0
#> 169                Minnesota 26  65  59  9 18  14   2   0  2   8  4   0
#> 170                  Toronto 52 195 166 28 48  36   7   2  3  19 26   0
#> 171                    Miami 52 224 193 25 60  46   8   1  5  36 22   3
#> 172                  Seattle 57 255 229 36 61  35  15   0 11  31 21   1
#> 173                    Miami 54 212 200 23 54  32   9   0 13  43 11   1
#> 174                  Chicago 30 104  90 11 24  18   1   0  5  19 12   1
#> 175                  Houston 26 122 109 15 32  24   3   2  3  11  7   0
#> 176             Philadelphia 38 156 132 25 31  13  10   3  5  18 16   0
#> 177                  Oakland 53 203 180 22 49  32  11   0  6  19 22   0
#> 178                 Colorado 26  93  89  8 27  18   5   2  2   7  4   0
#> 179              Los Angeles 54 204 189 20 52  33   8   1 10  32 11   1
#> 180                Tampa Bay 52 214 190 30 54  33  14   1  6  26 17   2
#> 181               Pittsburgh 39 159 147 22 39  24   7   0  8  24  8   0
#> 182                Tampa Bay 43 139 124 15 33  19  10   0  4  20 13   0
#> 183                  Oakland 51 227 207 29 57  35  13   2  7  36 15   0
#> 184              Los Angeles 28 110 102 10 31  19   9   2  1  12  4   0
#> 185                 New York 32  92  86 12 25  16   5   1  3  15  4   1
#> 186                  Chicago 39 144 126 17 34  23   6   0  5  21 16   1
#> 187                    Miami 41 168 159 20 45  26  13   0  6  18  8   0
#> 188                Milwaukee 37 142 118 14 28  17   5   0  6  18 18   0
#> 189                  Arizona 56 237 217 30 68  52  10   3  3  19 17   0
#> 190                Milwaukee 52 193 168 31 48  32  12   0  4  27 25   7
#> 191                  Detroit 52 226 214 33 66  47  10   3  6  27  8   0
#> 192            San Francisco 55 229 206 22 63  46  12   0  5  27 19   6
#> 193                Tampa Bay 51 190 179 22 54  41   5   2  6  22 10   0
#> 194                  Detroit 53 208 194 15 55  30  18   2  5  24 12   0
#> 195                  Houston 54 215 196 24 49  27   7   4 11  29 16   1
#> 196              Los Angeles 22  81  75  9 20  14   1   1  4  12  6   0
#> 197                Cleveland 47 178 158 31 41  25   8   3  5  19 19   0
#> 198                St. Louis 47 140 121 11 31  20   6   1  4   8 17   2
#> 199               Cincinnati 57 240 226 21 72  58   8   1  5  31 10   0
#> 200                  Atlanta 41 152 142 12 44  34   7   0  3  18  7   1
#> 201                 New York 43 144 137 20 40  27   8   0  5  17  6   1
#> 202                  Houston 42 130 119 15 34  25   3   1  5  12  6   0
#> 203                St. Louis 48 126 110 13 27  14   9   0  4  13 14   1
#> 204              Kansas City 50 221 204 35 60  43  11   1  5  26 11   1
#> 205              Los Angeles 46 130 119 14 35  25   6   1  3  13  9   1
#> 206                  Chicago 30  68  60  4 17  10   6   0  1  11  5   0
#> 207                    Miami 56 254 236 38 78  66   7   3  2  23 12   0
#> 208                  Chicago 37 123 105  7 30  24   4   0  2  12 12   0
#> 209              Kansas City 58 248 220 41 59  39  12   1  7  38 26   2
#> 210              Los Angeles 20  82  81 16 27  21   5   0  1  11  1   0
#> 211                Cleveland 50 193 175 30 46  27   9   5  5  20 16   0
#> 212                 Colorado 56 241 223 31 63  41  13   4  5  12 13   1
#> 213                    Miami 33  81  75  8 23  18   3   1  1   9  5   0
#> 214               Washington 57 235 209 24 56  38  10   0  8  30 22   0
#> 215                  Seattle 55 238 212 24 61  42  14   3  2  17 22   0
#> 216                Baltimore 57 226 215 23 62  40  14   0  8  24  5   0
#> 217               Pittsburgh 50 198 173 21 49  39   5   3  2  11 21   0
#> 218            San Francisco 52 188 174 22 52  41   6   3  2  20 13   0
#> 219 Los Angeles,Philadelphia 41 170 152 19 39  20  14   1  4  14 10   0
#> 220                  Atlanta 53 242 218 28 66  49  15   0  2  20 22   6
#> 221                 New York 23  67  60  9 16  10   4   0  2   4  5   0
#> 222                    Texas 53 211 195 26 51  29  10   4  8  27  8   2
#> 223                Baltimore 33 128 115 18 24   9   7   0  8  16 10   0
#> 224                Tampa Bay 16  52  45  5  9   3   3   0  3   7  4   0
#> 225                  Seattle 46 124 110 14 26  13   8   0  5  18 12   1
#> 226                  Toronto 19  74  68  5 19  14   3   0  2   7  6   0
#> 227               Washington 56 241 212 33 49  25  13   1 10  30 26   0
#> 228                 New York 35  94  87  9 25  17   6   0  2  11  4   0
#> 229                  Detroit 40 132 123 20 31  17   5   3  6  16  5   0
#> 230                    Texas 57 253 224 30 60  44   8   0  8  37 24   3
#> 231                    Texas 53 201 183 17 49  32  10   0  7  33 12   1
#> 232              Kansas City 34  89  88 11 24  10  10   1  3  10  0   0
#> 233               Pittsburgh 50 188 170 18 44  26  11   1  6  31 15   0
#> 234                  Toronto 44 149 137 20 31  13   9   0  9  31 10   0
#> 235               Pittsburgh 56 201 175 23 46  29   8   2  7  29 18   3
#> 236                  Toronto  9  25  22  3  6   4   1   0  1   2  1   0
#> 237            San Francisco 34 127 113 24 31  23   5   1  2   6 13   1
#> 238               Pittsburgh 56 233 213 33 59  40  12   1  6  27  8   1
#> 239              Los Angeles 41 140 125 15 35  25   8   0  2   8 14   0
#> 240                Cleveland 58 255 213 25 51  33  10   1  7  39 39   5
#> 241                Cleveland 17  61  51  8 12   9   1   0  2   5  7   0
#> 242                  Oakland 52 200 175 22 43  27   8   1  7  19 21   1
#> 243             Philadelphia 37 139 124 15 30  20   2   0  8  21 12   3
#> 244                  Chicago 21  49  42  7 11   8   2   1  0   4  7   0
#> 245               Cincinnati 56 237 221 30 60  37  14   1  8  30 10   0
#> 246                 New York 37 130 112 12 31  23   7   0  1   9 17   3
#> 247                  Chicago 55 228 211 25 59  41  13   0  5  27 17   1
#> 248                    Miami 51 183 158 23 39  24   8   3  4  16 15   1
#> 249                Tampa Bay 57 252 233 30 62  38  13   1 10  29 14   2
#> 250                San Diego 25  94  77 10 16  10   3   0  3  10 17   0
#> 251             Philadelphia 43 135 121 16 27  15   4   0  8  24 11   0
#> 252              Los Angeles  5   7   5  1  1   0   1   0  0   0  1   0
#> 253             Philadelphia 48 165 149 16 35  19   8   1  7  20 12   2
#> 254               Pittsburgh 37 119 109 17 32  24   7   1  0   5  8   0
#> 255                 New York 15  30  27  4  6   2   3   0  1   1  2   0
#> 256                  Toronto 55 243 223 35 71  60   9   1  1  19 13   0
#> 257                  Chicago 40  74  66  7 16  10   3   1  2   4  8   0
#> 258          Chicago,Seattle 52 183 173 21 49  33  12   1  3  23  9   0
#> 259                  Toronto 54 218 202 24 58  42  11   0  5  17 10   1
#> 260                  Oakland 40 140 125 14 31  18   8   1  4  13 14   1
#> 261      Atlanta,Kansas City 29  70  60  9 14   9   2   0  3  13  9   1
#> 262               Pittsburgh 54 250 232 32 64  45  11   3  5  22 17   1
#> 263                Baltimore 32 111  98 16 24  18   2   0  4  13 11   0
#> 264                Baltimore 37 145 129 11 34  26   4   0  4   9 15   0
#> 265                    Miami 30 111 104 12 31  25   2   3  1   9  6   0
#> 266                  Oakland  9  23  22  3  5   2   1   1  1   3  1   0
#> 267                  Chicago 55 200 180 28 45  26  11   1  7  28 16   2
#> 268               Washington  2  10   9  1  2   0   2   0  0   0  1   0
#> 269                  Chicago 26  79  75  4 22  15   6   0  1   4  4   1
#> 270                  Oakland 48 215 199 25 57  44   6   4  3  24 12   1
#> 271                Cleveland 51 162 145 17 40  30   7   0  3  25 16   2
#> 272     Boston,San Francisco 39 110  93 19 23  14   7   1  1   7 14   1
#> 273                San Diego 56 228 211 22 55  38   6   0 11  36 11   0
#> 274                 New York 41 123 114 16 30  20   6   0  4  12  9   1
#> 275                    Miami 15  51  44  8  9   3   3   1  2   8  6   0
#> 276                San Diego 51 178 160 19 43  28  12   1  2  12 14   1
#> 277               Washington 55 246 217 34 57  42  10   0  5  19 24   0
#> 278                  Detroit 21  73  58  4 12   8   3   0  1   2 15   0
#> 279                  Atlanta 47 160 155 14 43  27  10   0  6  21  3   0
#> 280                St. Louis 16  30  28  2  7   4   2   0  1   4  2   0
#> 281                  Toronto  9  15  12  1  3   2   1   0  0   0  2   1
#> 282                  Houston 44 113 100 19 24  15   4   1  4  15  7   0
#> 283                Minnesota 57 249 217 28 55  37  14   0  4  22 30   2
#> 284                Tampa Bay 30  86  72 11 14   7   3   0  4   6 10   0
#> 285                Milwaukee 44 165 154 20 41  24  14   0  3  18 10   1
#> 286                  Atlanta  4   5   4  0  1   1   0   0  0   0  1   0
#> 287                 New York 50 198 167 26 33  19   4   0 10  34 24   1
#> 288                  Chicago  4   8   8  1  2   0   2   0  0   3  0   0
#> 289                  Atlanta 46 149 118  8 23  14   5   0  4  17 27   0
#> 290               Cincinnati  4   4   4  0  1   0   1   0  0   2  0   0
#> 291                  Seattle  1   4   4  0  1   0   1   0  0   1  0   0
#> 292                 New York 58 224 202 23 55  41  10   0  4  26 14   0
#> 293                 Colorado 17  51  45  3 12   8   3   0  1   7  4   0
#> 294            San Francisco 59 264 248 33 71  54  12   2  3  30 15   0
#> 295                  Oakland 32 107  96 13 25  17   5   2  1  14  9   1
#> 296                Tampa Bay 12  29  27  3  6   3   1   0  2   4  1   0
#> 297                    Texas 21  69  65  6 17  12   1   0  4  10  2   0
#> 298                 Colorado 51 152 142 17 37  22  10   1  4  22  7   0
#> 299            San Francisco 15  35  34  3  8   4   2   0  2   4  1   0
#> 300                Tampa Bay 30  88  73  7 17  14   2   0  1   3 12   0
#> 301                San Diego 27 103  94 18 26  19   6   0  1   7  8   0
#> 302                Tampa Bay 51 182 172 10 52  41  10   0  1  15  8   2
#> 303                    Miami 47 166 155 17 42  30   6   2  4  18  7   1
#> 304             Philadelphia 22  61  58  9 15  10   3   0  2   5  3   0
#> 305              Los Angeles 39 157 131 19 31  22   6   0  3  16 18   1
#> 306                  Detroit 53 223 194 33 45  31   8   3  3  11 26   0
#> 307                  Atlanta 23  84  76  4 19  13   3   1  2  11  5   0
#> 308              Kansas City 48 192 181 19 50  35  10   0  5  26  6   3
#> 309                 New York 38 127 114 15 24  10   9   0  5  18 12   1
#> 310                    Texas 59 234 212 29 56  38  14   1  3  22 17   0
#> 311                 Colorado  6   9   7  1  2   2   0   0  0   0  1   0
#> 312                Baltimore 25  90  86 10 23  16   3   2  2  14  4   1
#> 313            San Francisco  3   8   7  0  2   2   0   0  0   0  1   0
#> 314            San Francisco 16  70  66  8 15   9   2   0  4  11  4   0
#> 315               Washington 25  40  36  5  9   7   1   0  1   1  4   0
#> 316            San Francisco  7  10   8  1  1   0   0   0  1   2  1   0
#> 317              Kansas City 18  49  40  3  9   6   2   0  1   3  5   0
#> 318              Los Angeles 59 259 239 32 54  34   6   1 13  27 16   0
#> 319                Cleveland 44 188 173 21 41  22  12   0  7  27  7   0
#> 320                Milwaukee 28  91  81  5 21  15   5   0  1  10  8   0
#> 321                  Detroit 40 148 137 11 36  24  10   0  2  15  9   0
#> 322                  Arizona 54 194 172 17 44  32   8   1  3  11 20   2
#> 323               Cincinnati 35  96  90  3 25  17   8   0  0   7  6   0
#> 324                  Toronto 38 151 131 19 25  13   4   0  8  28 17   1
#> 325            San Francisco 37 150 136 18 34  23   8   0  3  12 12   0
#> 326            San Francisco 43 170 155 15 36  17  14   0  5  20 12   5
#> 327                  Oakland 50 215 198 30 47  27  11   2  7  18 12   0
#> 328                Minnesota 57 235 211 25 47  27  11   1  8  31 19   0
#> 329                Baltimore 10  36  34  3  9   7   1   0  1   6  2   0
#> 330                  Chicago 58 252 232 24 60  40  14   0  6  28 16   2
#> 331                 Colorado 43 190 177 20 48  35   8   2  3  18  8   0
#> 332              Los Angeles 53 226 200 19 47  34   6   0  7  29 22   2
#> 333        Atlanta,Cleveland 31 110 106  9 30  24   4   0  2   9  4   0
#> 334                Minnesota 55 222 212 26 51  29   5   9  8  26  7   3
#> 335               Cincinnati 54 114 107 10 29  19   9   0  1  12  6   0
#> 336                  Toronto 17  26  23  4  6   3   3   0  0   5  2   0
#> 337                  Houston 40 158 144 19 36  23   9   0  4  13  8   1
#> 338              Los Angeles 46 154 148 15 40  29   6   0  5  23  3   0
#> 339                  Houston 27  89  82  8 17   8   4   0  5  18  5   0
#> 340              Kansas City 29  64  59 11 16  11   3   1  1   6  2   0
#> 341              Los Angeles 46 177 161 26 40  28   9   1  2   8 16   0
#> 342                  Seattle 11  24  21  6  4   2   1   0  1   3  3   0
#> 343               Cincinnati 48 171 157 27 39  29   5   2  3  10 13   0
#> 344                  Atlanta 26  86  82 14 22  17   2   1  2   5  3   0
#> 345                Milwaukee 50 171 168 23 48  36   9   1  2  11  3   0
#> 346                   Boston 20  76  70 10 19  15   3   0  1   7  3   0
#> 347                 New York 58 230 203 22 48  33  13   0  2  19 23   0
#> 348                  Arizona 38 126 112 12 29  18   9   0  2  15  9   0
#> 349                   Boston 45 187 169 22 45  33  10   2  0  20 14   0
#> 350                   Boston 48 191 179 25 47  34   8   2  3  22  9   0
#> 351              Kansas City 25 106  93  7 23  17   4   0  2   8  9   1
#> 352              Kansas City 46 180 167 14 44  28  12   2  2  14  8   0
#> 353                 New York 55 208 180 21 35  19   7   0  9  25 27   3
#> 354                 New York 38 119 107 16 24  15   4   1  4  16 10   1
#> 355                Baltimore 12  31  29  3  7   5   1   0  1   1  2   0
#> 356               Washington  7   3   3  1  1   1   0   0  0   0  0   0
#> 357                  Toronto  1   3   3  0  1   1   0   0  0   0  0   0
#> 358              Kansas City  1   3   3  0  1   1   0   0  0   0  0   0
#> 359                    Texas  1   3   3  0  1   1   0   0  0   0  0   0
#> 360                Baltimore  1   3   3  0  1   1   0   0  0   1  0   0
#> 361                Baltimore 46 194 185 21 42  25   7   0 10  32  6   0
#> 362              Los Angeles  5   7   6  1  2   2   0   0  0   0  0   0
#> 363             Philadelphia  3   3   3  0  1   1   0   0  0   0  0   0
#> 364                  Atlanta 45 177 162 22 42  33   6   1  2  11 14   0
#> 365                    Texas  5   6   6  1  2   2   0   0  0   0  0   0
#> 366                   Boston 29  76  66 10 19  17   1   0  1  10  5   1
#> 367                  Chicago 56 228 206 28 50  36   7   1  6  26 16   2
#> 368     Baltimore,Pittsburgh 18  31  27  1  5   1   3   0  1   8  4   1
#> 369                Minnesota 45 199 181 24 40  26   7   1  6  15 17   0
#> 370                  Detroit 19  57  49  4 10   6   2   0  2   5  6   0
#> 371                  Seattle 44 144 123 17 26  13   9   2  2  13 16   2
#> 372                 New York 42  96  81 13 17  12   2   1  2   9 13   1
#> 373                  Arizona 43 169 157 14 35  21   5   1  8  30  8   0
#> 374                  Atlanta 12  35  30  3  8   8   0   0  0   1  1   0
#> 375              Los Angeles 57 240 217 24 49  32   8   0  9  29 19   4
#> 376                    Miami 42 118 108 12 29  22   6   0  1  15  8   1
#> 377                  Arizona 42 139 131 15 31  17   7   4  3   9  5   0
#> 378 Cincinnati,San Francisco 54 220 207 18 50  28  14   3  5  35  8   1
#> 379                    Texas 53 235 205 37 49  35   8   4  2  19 21   0
#> 380                  Toronto 37 165 146 25 33  24   5   0  4  14 14   1
#> 381                St. Louis 27  77  72  6 17   9   5   1  2   8  3   0
#> 382                Minnesota 21  52  46  7 12   9   3   0  0   3  4   0
#> 383                Minnesota 47 170 155 15 40  32   6   0  2  20  9   0
#> 384            San Francisco 22  87  79  9 16   9   3   1  3   6  7   0
#> 385                Baltimore 35 122 115 12 26  14   8   0  4  18  5   0
#> 386            San Francisco 21  40  39  5 11   8   3   0  0   2  1   0
#> 387            San Francisco 35 111  91  9 18  11   6   1  0   8 14   0
#> 388                  Oakland  7  11  10  2  2   1   0   1  0   1  1   0
#> 389              Los Angeles 48 161 126 14 21  15   2   0  4   9 32   2
#> 390                Milwaukee 42 102  88 11 21  16   3   1  1   5  9   0
#> 391                Minnesota  8  14  12  1  3   3   0   0  0   2  2   0
#> 392               Washington 30  80  71 14 14   6   5   0  3   8  6   0
#> 393             Philadelphia 38 168 156 19 41  31   8   2  0  11 11   1
#> 394             Philadelphia 35 134 126 17 28  14   9   0  5  18  7   1
#> 395                    Miami  9  27  26  1  8   7   1   0  0   2  0   0
#> 396                 Colorado  6  11   9  2  2   2   0   0  0   0  2   0
#> 397                Cleveland 41 177 164 20 39  23  12   1  3  13 10   2
#> 398               Washington 13  17  12  3  1   0   0   0  1   2  3   0
#> 399                  Oakland 26  92  85 14 18  10   5   0  3  11  6   0
#> 400                 Colorado 53 219 198 28 51  42   6   1  2  19 17   2
#> 401                San Diego  7  12   9  2  2   1   1   0  0   0  1   0
#> 402                  Atlanta 46 178 165 11 46  38   8   0  0  12 11   2
#> 403              Los Angeles 14  40  37  1 10   9   1   0  0   1  3   0
#> 404             Philadelphia 42 123 118 12 29  21   4   0  4  12  4   0
#> 405                  Detroit  8  20  18  1  4   3   0   1  0   0  2   0
#> 406                  Atlanta 53 192 173 20 40  29   6   2  3  11 17   2
#> 407               Cincinnati 57 240 220 21 48  27  13   0  8  22 14   2
#> 408                    Miami 12  26  20  2  5   4   1   0  0   0  2   0
#> 409                  Houston 32 109  97 10 20  11   7   0  2   6 12   1
#> 410                Baltimore 29  84  74 10 12   6   1   0  5   9 10   1
#> 411             Philadelphia 11  21  20  3  5   3   2   0  0   2  1   0
#> 412                  Oakland 28  86  79  6 15   5   6   1  3  13  5   0
#> 413                St. Louis 35  66  57  6 12   8   3   0  1   2  7   1
#> 414                  Atlanta 15  36  32  1  6   3   2   0  1   3  4   0
#> 415                Minnesota 48 186 171 19 37  24   7   0  6  26 10   0
#> 416                Baltimore 13  36  35  4 10   7   3   0  0   3  0   0
#> 417                  Detroit 20  65  58  5 15  12   3   0  0   5  4   0
#> 418                  Oakland 30  86  80  7 20  15   5   0  0   6  6   0
#> 419                  Houston  3   5   3  2  0   0   0   0  0   0  2   0
#> 420             Philadelphia 36  97  84  9 14   7   3   1  3  10 13   0
#> 421               Cincinnati 41 137 121 12 30  24   6   0  0  10 11   0
#> 422                  Arizona 19  59  56  3 12   7   3   0  2   8  2   0
#> 423                    Texas 10  24  20  3  3   2   0   0  1   1  4   0
#> 424                St. Louis 53 209 189 19 47  40   5   0  2  18 16   2
#> 425                    Texas 28  88  77  5 17  11   5   0  1  10  7   0
#> 426               Pittsburgh 34 128 117 13 28  21   6   0  1  14 10   0
#> 427                Cleveland 34 107  98 13 24  18   5   0  1   8  6   0
#> 428                Minnesota 27  94  88 11 20  12   6   0  2   6  3   0
#> 429               Pittsburgh 18  27  24  1  6   6   0   0  0   1  3   0
#> 430                   Boston 18  45  43  7 11  10   0   0  1   3  2   0
#> 431                  Houston 48 182 164 23 33  18  10   0  5  20 15   1
#> 432                  Chicago 23  83  76  6 16  13   0   0  3   4  7   0
#> 433                Milwaukee 48 125 109  9 24  18   2   2  2   9 11   1
#> 434                 New York 54 239 220 27 51  38   8   1  4  15 13   1
#> 435             Philadelphia 55 225 206 24 52  44   3   2  3  23 10   1
#> 436                 New York  8  21  16  1  4   3   1   0  0   3  1   0
#> 437                 New York 54 230 202 22 42  34   3   0  5  19 23   0
#> 438              Los Angeles 13  37  28  2  5   3   1   0  1   7  7   2
#> 439               Pittsburgh 21  54  52  1 15  14   1   0  0   6  1   0
#> 440              Los Angeles 37 105  95  4 21  15   4   0  2  12  7   1
#> 441             Philadelphia 25  69  62  6 11   7   1   0  3  11  6   0
#> 442                Minnesota 57 257 233 27 48  30  11   1  6  21 21   1
#> 443                Cleveland 14  28  26  6  7   6   1   0  0   2  1   0
#> 444                  Chicago 51 184 166 20 36  24   9   0  3  17 13   0
#> 445                Tampa Bay 32  79  75  8 15   9   1   1  4  14  2   0
#> 446                  Detroit 49 190 176 12 41  29   7   0  5  28 10   2
#> 447                St. Louis 49 191 174 17 41  31   8   2  0  17 12   0
#> 448                 Colorado 23  57  52  4 13  11   1   1  0   3  3   1
#> 449                St. Louis 19  48  39  6  7   4   3   0  0   0  3   0
#> 450                Baltimore 54 233 220 28 51  34  12   0  5  18  7   1
#> 451              Los Angeles 56 230 220 24 56  42  12   1  1   9  5   0
#> 452                 Colorado 33  74  62 12 12   9   1   0  2   4 10   3
#> 453           Colorado,Texas 27  40  34  7  6   3   3   0  0   1  6   0
#> 454              Los Angeles 10  17  13  1  2   1   1   0  0   1  3   0
#> 455                  Atlanta 34  52  47  5 11   9   1   0  1   5  2   0
#> 456               Cincinnati  5   9   5  1  0   0   0   0  0   0  2   0
#> 457                  Arizona 55 197 183 20 41  26  13   1  1  18 12   1
#> 458                  Detroit 29 114 105 13 26  23   2   1  0   3  7   1
#> 459                  Detroit  6  14  13  0  3   1   2   0  0   1  0   0
#> 460               Cincinnati 44 122 111  4 24  16   6   2  0  14 10   0
#> 461               Cincinnati 22  80  72  9 16  13   2   0  1   4  7   0
#> 462               Washington 51 198 182 20 38  27   5   1  5  22 15   4
#> 463            San Francisco 13  43  39  1  9   6   3   0  0   5  3   0
#> 464                Milwaukee 55 226 218 23 53  39  10   2  2  22  4   0
#> 465          San Diego,Texas 46 120 103  8 22  18   4   0  0   8 13   2
#> 466                  Detroit 43 174 166 11 39  31   4   2  2  16  5   0
#> 467                   Boston 34 143 132 12 27  14  11   0  2  13  7   0
#> 468                St. Louis  9  21  20  1  4   1   3   0  0   4  0   0
#> 469                St. Louis 39 151 136  8 31  23   5   1  2  23 10   1
#> 470                Cleveland 38 106  99  8 20  13   3   1  3  13  6   0
#> 471          Arizona,Toronto 36 105  87 10 15   9   4   0  2  11 12   0
#> 472                 New York 16  62  57  5 10   7   0   0  3   6  5   0
#> 473        Atlanta,Cleveland 48 163 142  8 31  28   2   1  0  10 18   0
#> 474               Washington 50 189 176 12 38  28   4   0  6  26  9   1
#> 475                  Oakland  5   7   7  0  2   2   0   0  0   0  0   0
#> 476                  Seattle 32  99  98  9 20  11   5   0  4  16  1   0
#> 477                Minnesota  3   5   5  0  1   0   1   0  0   0  0   0
#> 478                Milwaukee 44 128 127  7 33  26   6   1  0  17  0   0
#> 479              Los Angeles 32  87  75  8 16  13   1   2  0   3  5   1
#> 480                  Chicago 11  37  36  3  7   4   1   1  1   1  1   0
#> 481               Washington 13  29  25  0  6   6   0   0  0   0  2   0
#> 482              Kansas City 11  29  26  4  4   2   1   0  1   4  3   0
#> 483              Los Angeles 25  80  72  6 12   5   5   0  2   8  6   0
#> 484               Washington 24  47  45  5  8   4   2   0  2   5  1   0
#> 485            San Francisco 12  26  24  4  4   1   2   0  1   3  1   0
#> 486                San Diego 32  92  86  8 18  13   2   2  1   9  4   0
#> 487                Cleveland 35 135 122 12 22  15   4   0  3  10 11   0
#> 488              Los Angeles 25  52  46  8  8   5   2   0  1   4  5   0
#> 489                St. Louis 21  23  20  2  4   4   0   0  0   0  3   0
#> 490                    Miami 53 176 159 22 34  27   4   3  0   6 12   0
#> 491                  Chicago 32 114 105 11 18  10   5   0  3   8  9   0
#> 492                  Chicago 46 168 159 20 33  24   5   2  2  14  6   0
#> 493               Washington 10  26  22  1  5   5   0   0  0   0  2   0
#> 494               Cincinnati 59 245 228 30 40  18  12   1  9  28 13   3
#> 495                 New York 13  43  36  6  5   3   1   0  1   4  7   1
#> 496                Milwaukee 11  21  20  1  5   4   1   0  0   0  0   0
#> 497                  Houston 30  82  77  6 13   7   3   0  3   5  3   0
#> 498                  Arizona 36 114 110 11 22  16   2   1  3   9  4   0
#> 499                San Diego 40 103  98  9 21  14   4   2  1   9  3   1
#> 500                  Oakland 33 101  90 10 16  11   4   0  1   7  9   0
#> 501               Cincinnati 19  35  33  2  6   4   1   0  1   4  1   0
#> 502              Los Angeles 31  53  50  4 11   9   2   0  0   4  2   0
#> 503                  Detroit 34  98  89 10 19  17   2   0  0   6  4   0
#> 504                  Chicago 23  69  63  7 11   8   1   0  2   3  5   0
#> 505                  Atlanta 19  56  54  4 11   8   2   0  1   3  2   0
#> 506            San Francisco 21  47  40  2  7   6   1   0  0   1  6   1
#> 507              Kansas City 55 241 222 23 48  41   3   3  1  11  8   0
#> 508              Los Angeles  7  25  22  3  4   2   1   1  0   1  2   1
#> 509                  Toronto  2   3   2  0  0   0   0   0  0   0  1   0
#> 510                  Chicago  3   3   2  0  0   0   0   0  0   0  1   0
#> 511                 Colorado 42 111 105 10 18  11   3   1  3  11  6   0
#> 512                  Toronto  2   7   6  1  1   1   0   0  0   1  0   0
#> 513              Los Angeles 34  89  82 12 15  11   2   1  1   4  3   0
#> 514                  Arizona 11  14  12  1  2   2   0   0  0   0  1   0
#> 515             Philadelphia 11  18  17  2  3   2   0   1  0   0  1   0
#> 516                  Toronto 10  16  12  4  2   2   0   0  0   1  2   0
#> 517                Baltimore 25  67  64  5 13   9   4   0  0   3  3   0
#> 518            San Francisco  9  30  28  2  6   5   0   1  0   1  0   0
#> 519                  Oakland 11  38  35  0  6   3   3   0  0   2  3   0
#> 520                  Chicago 12  11  10  0  2   2   0   0  0   1  1   0
#> 521               Washington 14  53  47  4  9   6   3   0  0   9  4   0
#> 522             Philadelphia 21  75  70  5 13   9   3   1  0   4  4   0
#> 523                    Texas 27  51  45  8  9   8   0   0  1   3  3   0
#> 524                    Miami 10  19  16  0  4   4   0   0  0   1  0   0
#> 525              Los Angeles 12   8   8  4  2   2   0   0  0   0  0   0
#> 526                San Diego  8   8   8  0  2   2   0   0  0   0  0   0
#> 527                  Detroit  2   4   4  0  1   1   0   0  0   0  0   0
#> 528              Los Angeles 34 130 107  5 13  10   1   0  2   8 20   0
#> 529                Cleveland  1   4   4  0  1   1   0   0  0   1  0   0
#> 530                   Boston  7  23  22  1  5   4   1   0  0   0  0   0
#> 531                 Colorado 21  72  66  5 10   5   3   0  2   8  6   2
#> 532                 Colorado 38  89  84  8 16  11   3   1  1   6  3   2
#> 533                  Oakland 11  35  31  3  4   3   0   0  1   3  4   0
#> 534                Tampa Bay 23  61  56  2 11   9   2   0  0   2  2   0
#> 535                  Seattle 29 100  89  7 16  11   5   0  0   5  6   0
#> 536                 New York  9  19  17  1  3   3   0   0  0   2  2   0
#> 537              Kansas City 27  92  88  4 15   8   3   3  1  15  2   0
#> 538                 New York  8  17  15  0  3   3   0   0  0   2  1   0
#> 539                  Chicago 18  22  22  2  3   0   2   0  1   1  0   0
#> 540              Los Angeles 12  47  44  1  7   4   2   0  1   5  3   1
#> 541              Los Angeles  8  22  21  1  3   2   0   0  1   1  1   0
#> 542                  Chicago  9  31  26  2  3   1   2   0  0   1  4   0
#> 543            San Francisco  7  13  11  1  2   2   0   0  0   0  1   0
#> 544                 New York 29  66  61  5 11   9   2   0  0   1  4   0
#> 545               Washington 12  28  26  3  6   6   0   0  0   0  0   0
#> 546                Baltimore 40 154 144 10 25  19   5   0  1   7  8   0
#> 547            San Francisco 12  20  19  2  2   0   1   0  1   3  1   0
#> 548              Los Angeles 10  26  22  1  4   3   1   0  0   0  1   0
#> 549               Pittsburgh 13  15  13  4  1   0   0   1  0   1  2   0
#> 550                San Diego 28  81  73  8 12  10   1   0  1   5  6   1
#> 551                  Chicago 11  33  30  3  4   2   1   0  1   6  2   0
#> 552            San Francisco  7  19  16  2  2   1   1   0  0   0  2   0
#> 553            San Francisco 11  25  21  0  2   1   1   0  0   3  4   0
#> 554                   Boston 14  58  55  5  9   4   5   0  0   4  1   0
#> 555                    Miami 42  69  65  3 11   9   2   0  0   5  4   0
#> 556                 Colorado  8  14  14  1  3   3   0   0  0   1  0   0
#> 557                Minnesota 16  41  39  3  5   2   2   0  1   2  1   0
#> 558                   Boston 11  28  26  1  4   3   1   0  0   1  2   0
#> 559                St. Louis  9  19  18  0  3   1   2   0  0   0  0   0
#> 560                San Diego 32  63  59  5  9   5   4   0  0   6  2   0
#> 561                  Arizona 10  22  20  1  3   3   0   0  0   2  2   0
#> 562                  Chicago 10  29  23  6  3   3   0   0  0   0  3   0
#> 563              Kansas City  9   4   3  1  0   0   0   0  0   0  0   0
#> 564                  Chicago 22  67  61  3 10   8   2   0  0   4  4   1
#> 565                  Seattle 14  17  17  1  3   2   1   0  0   0  0   0
#> 566               Cincinnati  8   9   8  1  1   1   0   0  0   0  0   0
#> 567               Washington  6   7   5  1  1   1   0   0  0   0  0   0
#> 568                 New York  4   5   5  1  1   1   0   0  0   0  0   0
#> 569               Washington  5   5   4  0  1   1   0   0  0   1  0   0
#> 570                 New York  9  25  23  0  3   2   1   0  0   2  2   0
#> 571                Milwaukee 13  33  28  1  4   4   0   0  0   3  4   1
#> 572                St. Louis 32  31  27  6  2   0   0   0  2   2  0   0
#> 573                Tampa Bay 18  38  38  3  7   6   1   0  0   2  0   0
#> 574                  Arizona  3   4   3  0  0   0   0   0  0   0  1   0
#> 575                Baltimore  8  22  22  2  3   0   3   0  0   0  0   0
#> 576              Los Angeles 25  55  51  3  7   6   1   0  0   4  2   0
#> 577                St. Louis 11  26  24  0  4   4   0   0  0   2  1   0
#> 578               Washington  8  17  14  2  2   2   0   0  0   0  0   0
#> 579                St. Louis 10  21  17  1  2   2   0   0  0   0  2   0
#> 580                  Seattle 18  60  54  4  7   6   0   0  1   2  3   0
#> 581                 Colorado 20  43  42  5  7   7   0   0  0   0  0   0
#> 582                  Atlanta 12  28  20  0  3   3   0   0  0   0  1   0
#> 583                Minnesota 15  10   9  1  1   1   0   0  0   0  1   0
#> 584                  Chicago  9  21  15  1  1   1   0   0  0   1  3   0
#> 585                 Colorado 13  24  23  3  4   4   0   0  0   0  0   0
#> 586                Baltimore  6   9   7  0  0   0   0   0  0   0  2   0
#> 587     Cincinnati,Cleveland  7  21  19  3  2   2   0   0  0   0  2   0
#> 588                    Miami  5  10   6  0  1   1   0   0  0   0  0   0
#> 589                    Texas  3   6   6  0  1   1   0   0  0   0  0   0
#> 590              Los Angeles  4   6   6  0  1   1   0   0  0   0  0   0
#> 591               Washington  6   7   6  0  1   1   0   0  0   0  0   0
#> 592                 New York 10  25  22  2  3   3   0   0  0   1  0   0
#> 593                  Atlanta 11  23  16  0  1   0   1   0  0   0  2   0
#> 594                    Miami 15  25  23  1  3   3   0   0  0   0  1   1
#> 595                Tampa Bay 13  31  31  1  4   2   2   0  0   2  0   0
#> 596                    Texas 12  26  26  3  4   4   0   0  0   1  0   0
#> 597                Milwaukee  9  21  20  2  2   1   1   0  0   1  0   0
#> 598               Pittsburgh 12  29  26  1  4   4   0   0  0   2  0   0
#> 599                  Chicago 11  22  20  3  3   3   0   0  0   2  0   0
#> 600               Cincinnati  9  19  19  0  2   1   0   1  0   1  0   0
#> 601             Philadelphia  7  16  14  1  2   2   0   0  0   2  0   0
#> 602                  Arizona  5   8   7  0  1   1   0   0  0   0  0   0
#> 603                Milwaukee  7  16  14  0  2   2   0   0  0   0  0   0
#> 604               Washington 12  24  19  0  2   2   0   0  0   0  1   0
#> 605            San Francisco 10  17  17  1  1   0   0   0  1   3  0   0
#> 606                  Toronto  8  22  22  1  3   3   0   0  0   0  0   0
#> 607                  Atlanta 10  18  15  0  2   2   0   0  0   0  0   0
#> 608              Los Angeles  6   6   5  0  0   0   0   0  0   0  1   0
#> 609                 New York  2   6   5  0  0   0   0   0  0   0  1   0
#> 610                San Diego 11  27  24  1  2   1   1   0  0   2  1   0
#> 611                St. Louis 12  29  26  0  2   2   0   0  0   0  2   0
#> 612                  Arizona  5   9   8  0  1   1   0   0  0   0  0   0
#> 613                 New York  4   8   8  1  1   1   0   0  0   0  0   0
#> 614              Los Angeles 10  24  22  0  2   2   0   0  0   0  0   0
#> 615               Cincinnati 11  23  22  0  2   2   0   0  0   1  1   0
#> 616                 Colorado  8  13  11  1  0   0   0   0  0   0  2   0
#> 617                Milwaukee  9  16  14  0  1   1   0   0  0   0  1   0
#> 618              Los Angeles 10  21  18  1  2   2   0   0  0   0  0   0
#> 619                San Diego  7   9   9  0  1   1   0   0  0   0  0   0
#> 620             Philadelphia 11  21  18  2  2   2   0   0  0   0  0   0
#> 621                St. Louis  5   9   6  0  0   0   0   0  0   0  1   0
#> 622                    Texas  2   7   6  0  0   0   0   0  0   0  1   0
#> 623                  Arizona 10  20  19  2  2   2   0   0  0   0  0   0
#> 624                  Seattle 12  34  32  1  2   1   1   0  0   1  1   0
#> 625                    Miami  8  17  11  0  1   1   0   0  0   2  0   0
#> 626            San Francisco  9  13  11  0  1   1   0   0  0   0  0   0
#> 627                Baltimore  4  11  11  0  1   1   0   0  0   0  0   0
#> 628                 New York 18   9   8  9  0   0   0   0  0   0  0   0
#> 629               Cincinnati  9  16  16  0  1   0   1   0  0   1  0   0
#> 630                  Atlanta 10  12  12  3  1   1   0   0  0   0  0   0
#> 631               Cincinnati 15  25  24  2  2   2   0   0  0   0  0   0
#> 632              Los Angeles  9  14   9  0  0   0   0   0  0   0  1   0
#> 633                  Oakland  6  10   9  0  0   0   0   0  0   0  1   0
#> 634                Milwaukee  6  14  13  1  1   1   0   0  0   0  0   0
#> 635                Milwaukee  5  15  14  0  1   1   0   0  0   0  0   0
#> 636             Philadelphia  8  16  15  0  1   1   0   0  0   1  0   0
#> 637                  Arizona  9  18  17  0  1   1   0   0  0   1  0   0
#> 638                 Colorado  9  18  17  0  1   1   0   0  0   0  0   0
#> 639                St. Louis  9  18  18  1  1   1   0   0  0   0  0   0
#> 640                  Atlanta  9  18  13  1  0   0   0   0  0   0  1   0
#> 641                  Arizona 10  19  18  0  1   1   0   0  0   0  0   0
#> 642               Pittsburgh 10  23  20  1  1   1   0   0  0   0  0   0
#> 643                San Diego 13  21  21  1  1   1   0   0  0   0  0   0
#> 644                    Miami 11  23  21  0  1   1   0   0  0   0  0   0
#> 645            San Francisco  1   1   1  0  0   0   0   0  0   0  0   0
#> 646              Los Angeles  1   1   1  0  0   0   0   0  0   0  0   0
#> 647               Cincinnati  1   2   2  0  0   0   0   0  0   0  0   0
#> 648               Cincinnati  1   1   1  0  0   0   0   0  0   0  0   0
#> 649               Cincinnati  1   1   1  0  0   0   0   0  0   0  0   0
#> 650                  Atlanta  1   1   1  0  0   0   0   0  0   0  0   0
#> 651                  Seattle  4  11  11  0  0   0   0   0  0   0  0   0
#> 652               Pittsburgh  1   1   1  0  0   0   0   0  0   0  0   0
#> 653                  Chicago  7   1   1  1  0   0   0   0  0   0  0   0
#> 654               Pittsburgh  3   3   3  0  0   0   0   0  0   0  0   0
#> 655                Milwaukee  1   1   1  0  0   0   0   0  0   0  0   0
#> 656              Los Angeles  5   7   6  0  0   0   0   0  0   0  0   0
#> 657                  Chicago  2   1   1  0  0   0   0   0  0   0  0   0
#> 658                  Atlanta  1   1   1  0  0   0   0   0  0   0  0   0
#> 659                  Oakland  1   2   2  0  0   0   0   0  0   0  0   0
#> 660               Pittsburgh  7   2   2  3  0   0   0   0  0   0  0   0
#> 661             Philadelphia  6   6   6  0  0   0   0   0  0   0  0   0
#> 662               Pittsburgh  5   9   7  0  0   0   0   0  0   0  0   0
#> 663                 Colorado  3   4   4  0  0   0   0   0  0   0  0   0
#> 664            San Francisco  5  10   7  0  0   0   0   0  0   1  0   0
#> 665                   Boston  2   4   4  0  0   0   0   0  0   0  0   0
#> 666               Cincinnati  1   1   1  1  0   0   0   0  0   0  0   0
#> 667                 New York  1   1   1  0  0   0   0   0  0   0  0   0
#> 668                    Miami  5   5   4  0  0   0   0   0  0   0  0   0
#> 669                Milwaukee  4   6   5  0  0   0   0   0  0   0  0   0
#> 670             Philadelphia  4   4   4  0  0   0   0   0  0   0  0   0
#> 671                San Diego  7   9   8  0  0   0   0   0  0   1  0   0
#> 672                San Diego  3   6   6  0  0   0   0   0  0   0  0   0
#> 673                  Atlanta  1   1   1  0  0   0   0   0  0   0  0   0
#> 674               Pittsburgh  2   2   2  0  0   0   0   0  0   0  0   0
#> 675                  Toronto  1   2   1  0  0   0   0   0  0   0  0   0
#> 676                  Chicago  1   1   1  0  0   0   0   0  0   0  0   0
#> 677                 New York  1   3   3  1  0   0   0   0  0   0  0   0
#> 678                San Diego  3   6   5  0  0   0   0   0  0   0  0   0
#> 679               Cincinnati  4   6   4  0  0   0   0   0  0   0  0   0
#> 680                    Miami  1   1   1  0  0   0   0   0  0   0  0   0
#> 681                  Atlanta  5   8   8  0  0   0   0   0  0   0  0   0
#> 682              Los Angeles  1   1   1  0  0   0   0   0  0   0  0   0
#> 683                Milwaukee  7  13  10  0  0   0   0   0  0   0  0   0
#> 684                 New York  1   1   1  0  0   0   0   0  0   0  0   0
#> 685                Milwaukee  2   4   3  0  0   0   0   0  0   0  0   0
#> 686                Milwaukee  3   3   3  0  0   0   0   0  0   0  0   0
#> 687             Philadelphia  1   1   1  0  0   0   0   0  0   0  0   0
#> 688                  Oakland  1   3   3  0  0   0   0   0  0   0  0   0
#> 689                 Colorado  5   9   8  0  0   0   0   0  0   0  0   0
#> 690                  Detroit  1   1   1  0  0   0   0   0  0   0  0   0
#> 691                 New York  9  21  21  0  0   0   0   0  0   0  0   0
#> 692              Los Angeles  1   1   1  0  0   0   0   0  0   0  0   0
#> 693              Los Angeles  1   2   2  0  0   0   0   0  0   0  0   0
#> 694                  Chicago 11  22  21  0  0   0   0   0  0   0  0   0
#> 695                  Seattle  1   3   3  0  0   0   0   0  0   0  0   0
#> 696                  Atlanta  1   1   1  0  0   0   0   0  0   0  0   0
#> 697                  Houston  1   2   2  0  0   0   0   0  0   0  0   0
#> 698                San Diego  2   3   2  0  0   0   0   0  0   0  0   0
#> 699                   Boston  1   3   3  0  0   0   0   0  0   0  0   0
#> 700                San Diego 11  18  14  0  0   0   0   0  0   0  0   0
#> 701                Minnesota  2   2   2  0  0   0   0   0  0   0  0   0
#> 702                  Houston  1   3   2  0  0   0   0   0  0   0  0   0
#> 703                San Diego  1   1   1  0  0   0   0   0  0   0  0   0
#> 704                Cleveland  1   2   2  0  0   0   0   0  0   0  0   0
#> 705            San Francisco  1   1   1  0  0   0   0   0  0   0  0   0
#> 706              Los Angeles  2   1   1  0  0   0   0   0  0   0  0   0
#> 707               Cincinnati  1   1   1  0  0   0   0   0  0   0  0   0
#> 708                    Texas  1   2   2  0  0   0   0   0  0   0  0   0
#> 709               Pittsburgh 10  21  18  1  0   0   0   0  0   0  0   0
#> 710                Milwaukee  1   2   2  0  0   0   0   0  0   0  0   0
#> 711                St. Louis  1   1   1  0  0   0   0   0  0   0  0   0
#> 712                  Houston  1   3   3  0  0   0   0   0  0   0  0   0
#> 713                  Atlanta  1   1   1  0  0   0   0   0  0   0  0   0
#> 714                   Boston  1   2   2  0  0   0   0   0  0   0  0   0
#> 715                 New York  1   1   1  0  0   0   0   0  0   0  0   0
#> 716                 New York  2   2   2  0  0   0   0   0  0   0  0   0
#> 717               Pittsburgh 10  23  21  0  0   0   0   0  0   0  0   0
#> 718                    Miami  6   7   6  1  0   0   0   0  0   1  0   0
#> 719               Cincinnati  1   1   1  0  0   0   0   0  0   0  0   0
#> 720             Philadelphia  1   1   1  0  0   0   0   0  0   0  0   0
#> 721             Philadelphia 11  20  18  0  0   0   0   0  0   0  0   0
#> 722                   Boston  1   2   2  0  0   0   0   0  0   0  0   0
#> 723                Milwaukee  6   7   7  0  0   0   0   0  0   0  0   0
#> 724              Kansas City  3   5   5  0  0   0   0   0  0   0  0   0
#> 725                    Miami  2   2   2  0  0   0   0   0  0   0  0   0
#> 726                 New York  1   2   2  0  0   0   0   0  0   0  0   0
#> 727                  Toronto  1   4   4  0  0   0   0   0  0   0  0   0
#> 728              Los Angeles  1   1   1  0  0   0   0   0  0   0  0   0
#> 729                  Arizona  1   1   1  0  0   0   0   0  0   0  0   0
#> 730                    Miami  1   1   1  0  0   0   0   0  0   0  0   0
#> 731              Los Angeles  5   2   2  1  0   0   0   0  0   0  0   0
#> 732                   Boston  1   3   2  0  0   0   0   0  0   0  0   0
#> 733                 New York  1   2   2  0  0   0   0   0  0   0  0   0
#> 734                  Chicago  1   1   1  0  0   0   0   0  0   0  0   0
#> 735                 New York  1   3   3  0  0   0   0   0  0   0  0   0
#> 736               Cincinnati 11  17  16  1  0   0   0   0  0   0  0   0
#> 737                  Detroit  1   2   2  0  0   0   0   0  0   0  0   0
#> 738                 New York  1   1   1  0  0   0   0   0  0   0  0   0
#> 739                 New York  1   2   2  0  0   0   0   0  0   0  0   0
#> 740              Los Angeles  1   1   1  0  0   0   0   0  0   0  0   0
#> 741                Milwaukee  3   3   3  0  0   0   0   0  0   0  0   0
#> 742                Baltimore  1   2   2  0  0   0   0   0  0   0  0   0
#> 743               Washington  2   2   2  0  0   0   0   0  0   0  0   0
#> 744                    Miami  2   2   2  0  0   0   0   0  0   0  0   0
#> 745                  Oakland  1   1   1  0  0   0   0   0  0   0  0   0
#> 746              Kansas City  1   2   2  0  0   0   0   0  0   0  0   0
#> 747                 New York  4   7   6  0  0   0   0   0  0   0  0   0
#> 748                St. Louis  2   4   3  0  0   0   0   0  0   0  0   0
#> 749              Kansas City  1   2   2  0  0   0   0   0  0   0  0   0
#> 750                Milwaukee  2   2   2  0  0   0   0   0  0   0  0   0
#> 751               Cincinnati  1   1   1  0  0   0   0   0  0   0  0   0
#> 752                Cleveland  1   3   3  0  0   0   0   0  0   0  0   0
#> 753                  Atlanta  5  10  10  0  0   0   0   0  0   0  0   0
#> 754                  Arizona  2   3   3  1  0   0   0   0  0   0  0   0
#> 755             Philadelphia  6  10   8  0  0   0   0   0  0   0  0   0
#> 756                Baltimore  1   2   2  0  0   0   0   0  0   0  0   0
#> 757                  Chicago  3   3   3  0  0   0   0   0  0   0  0   0
#> 758                   Boston  1   2   1  0  0   0   0   0  0   0  0   0
#> 759                    Miami  1   1   0  0  0   0   0   0  0   0  0   0
#> 760                  Arizona  1   1   0  0  0   0   0   0  0   0  0   0
#> 761              Kansas City  1   1   0  1  0   0   0   0  0   0  0   0
#> 762                 Colorado  1   1   0  0  0   0   0   0  0   0  0   0
#> 763              Los Angeles  1   0   0  0  0   0   0   0  0   0  0   0
#> 764            San Francisco  2   2   0  0  0   0   0   0  0   0  0   0
#>     uBB SO HBP SH SF GDP SB CS    BA   OBP   SLG   OPS  wOBA wOBA_CON
#> 1     0  0   0  0  0   0  0  0 1.000 1.000 2.000 3.000 1.256    1.256
#> 2     0  0   0  0  0   0  0  0 1.000 1.000 2.000 3.000 1.256    1.256
#> 3     0  0   0  0  0   0  0  0 0.500 0.500 2.000 2.500 1.032    1.032
#> 4     1  1   0  0  0   0  0  0 0.500 0.667 2.000 2.667 0.917    2.065
#> 5     0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000 0.881    0.881
#> 6     0  0   0  0  0   0  5  2 1.000 1.000 1.000 2.000 0.881    0.881
#> 7     0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000 0.881    0.881
#> 8     1  0   0  0  0   0  0  0    NA 1.000    NA    NA 0.687      NaN
#> 9     0  1   0  0  0   0  0  0 0.500 0.500 1.000 1.500 0.628    1.256
#> 10    1  0   1  0  0   0  0  0 0.417 0.500 1.000 1.500 0.607    0.591
#> 11    2  2   0  0  1   0  0  1 0.500 0.545 1.000 1.545 0.587    0.847
#> 12    2  1   0  0  0   0  0  0 0.429 0.556 0.857 1.413 0.578    0.638
#> 13    5 16   0  0  0   0  1  0 0.389 0.463 0.944 1.408 0.576    1.009
#> 14    2  1   0  0  0   0  0  0 0.375 0.500 0.875 1.375 0.558    0.600
#> 15    7 10   2  0  0   1  0  0 0.400 0.510 0.775 1.285 0.535    0.665
#> 16    0  2   0  1  0   0  0  0 0.333 0.333 0.833 1.167 0.491    0.736
#> 17   23 28   5  0  4   3  2  0 0.350 0.440 0.761 1.201 0.490    0.555
#> 18    2  3   1  0  0   0  0  0 0.333 0.467 0.667 1.133 0.478    0.565
#> 19    2  5   0  0  0   0  0  0 0.333 0.455 0.667 1.121 0.473    0.957
#> 20    8 36   3  0  0   4  0  0 0.315 0.383 0.704 1.087 0.454    0.645
#> 21   46 48   2  0  3   8  2  0 0.332 0.468 0.601 1.069 0.450    0.529
#> 22    2  2   0  0  0   1  3  0 0.381 0.435 0.619 1.054 0.450    0.473
#> 23   21 35   0  0  6   6  0  0 0.324 0.404 0.709 1.113 0.449    0.541
#> 24    1  5   0  1  0   1  0  0 0.400 0.438 0.600 1.038 0.447    0.647
#> 25    0  6   0  3  0   0  0  0 0.350 0.350 0.700 1.050 0.446    0.636
#> 26   10 17   1  0  0   1  0  0 0.381 0.473 0.556 1.029 0.445    0.551
#> 27   56 58   2  0  1   3  5  1 0.317 0.498 0.552 1.050 0.445    0.543
#> 28    0  0   0  1  0   0  0  0 0.500 0.500 0.500 1.000 0.440    0.440
#> 29    0  1   0  0  0   0  0  0 0.500 0.500 0.500 1.000 0.440    0.881
#> 30    0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000 0.440    0.440
#> 31    0  1   0  0  0   0  0  0 0.500 0.500 0.500 1.000 0.440    0.881
#> 32    0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000 0.440    0.440
#> 33    0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000 0.440    0.440
#> 34   35 79   6  0  0   2  2  2 0.279 0.407 0.630 1.037 0.434    0.617
#> 35    0  2   0  0  0   0  0  0 0.429 0.429 0.571 1.000 0.431    0.604
#> 36   38 42   1  0  1   8  4  0 0.283 0.396 0.624 1.020 0.430    0.481
#> 37   38 51   8  1  1   1  2  0 0.336 0.456 0.540 0.996 0.430    0.495
#> 38    4  8   0  1  0   0  0  1 0.327 0.377 0.633 1.010 0.427    0.485
#> 39    0  3   0  0  0   0  0  0 0.400 0.400 0.600 1.000 0.427    1.068
#> 40   14 33   0  0  5   3  0  0 0.317 0.374 0.658 1.032 0.425    0.568
#> 41    4 23   2  0  0   0  4  1 0.316 0.366 0.632 0.997 0.423    0.575
#> 42   34 46   2  1  5   5  3  0 0.305 0.398 0.610 1.008 0.422    0.494
#> 43    0  1   0  0  0   0  0  0 0.333 0.333 0.667 1.000 0.419    0.628
#> 44    1  1   0  0  0   1  3  1 0.400 0.455 0.500 0.955 0.417    0.433
#> 45    3 13   2  0  0   1  2  0 0.296 0.356 0.630 0.986 0.416    0.513
#> 46    6 34   2  0  0   3  0  0 0.337 0.388 0.579 0.967 0.414    0.608
#> 47    1  2   0  0  0   0  0  0 0.375 0.444 0.500 0.944 0.412    0.503
#> 48   16 30   1  0  2   1  8  2 0.348 0.395 0.569 0.963 0.410    0.459
#> 49   21 75   4  0  0   3  3  1 0.319 0.390 0.565 0.955 0.409    0.577
#> 50    2  2   0  1  0   0  0  0 0.400 0.500 0.400 0.900 0.408    0.440
#> 51   27 39   2  0  1   6  0  0 0.309 0.404 0.555 0.959 0.408    0.461
#> 52   14 40   0  0  3   3  4  2 0.358 0.401 0.559 0.960 0.408    0.506
#> 53    4  7   0  0  1   0  0  0 0.283 0.333 0.652 0.986 0.407    0.462
#> 54   18 29   1  0  1   3  0  1 0.264 0.388 0.591 0.979 0.405    0.489
#> 55    2  5   0  0  0   2  2  0 0.355 0.412 0.548 0.960 0.405    0.462
#> 56   18 38   1 11  5   4 10  2 0.353 0.395 0.558 0.953 0.404    0.468
#> 57   25 72   2  0  1   2  1  1 0.282 0.363 0.593 0.956 0.403    0.553
#> 58    4  1   0  0  0   0  1  0 0.167 0.500 0.333 0.833 0.400    0.251
#> 59   18 29   1  0  1   6  9  2 0.329 0.405 0.526 0.931 0.399    0.452
#> 60    3  8   1  0  1   0  0  0 0.275 0.348 0.625 0.973 0.398    0.472
#> 61    0  2   0  0  0   0  0  0 0.250 0.250 0.750 1.000 0.398    0.797
#> 62    3 19   2  1  1   2  1  2 0.344 0.371 0.570 0.941 0.397    0.446
#> 63   11 30   0  0  1   5  1  0 0.331 0.381 0.551 0.932 0.397    0.491
#> 64   10 28   1  1  0   3  0  0 0.287 0.357 0.574 0.931 0.397    0.505
#> 65   12 28   1  0  1   3  0  1 0.256 0.354 0.585 0.940 0.396    0.538
#> 66   12 19   1  0  0   2  2  0 0.316 0.404 0.516 0.919 0.395    0.443
#> 67    9 10   0  0  0   1  1  1 0.320 0.393 0.520 0.913 0.394    0.414
#> 68   12 28   0  0  0   7  6  3 0.361 0.411 0.506 0.917 0.394    0.448
#> 69   11 54   4  0  1   5  4  0 0.286 0.337 0.604 0.941 0.393    0.492
#> 70    7  9   2  0  1   2  0  1 0.295 0.380 0.541 0.921 0.393    0.416
#> 71   11 21   1  0  1   5  4  0 0.331 0.383 0.541 0.924 0.391    0.425
#> 72    3  5   0  0  0   0  1  1 0.393 0.452 0.429 0.880 0.391    0.438
#> 73   40 54   4  0  3   5  6  2 0.293 0.433 0.480 0.912 0.391    0.454
#> 74    2  6   0  0  0   0  0  0 0.318 0.375 0.545 0.920 0.391    0.500
#> 75   17 31   3  1  1   5  0  0 0.284 0.354 0.563 0.917 0.389    0.429
#> 76    1 22   0  0  1   1  0  0 0.310 0.315 0.606 0.921 0.388    0.563
#> 77   10 49   0  0  5   5  2  2 0.305 0.335 0.597 0.931 0.387    0.484
#> 78   23 56   2  1  2   5  3  0 0.271 0.358 0.559 0.917 0.387    0.511
#> 79    5 17   0  1  0   2  0  0 0.341 0.378 0.518 0.895 0.387    0.462
#> 80   18 25   1  0  5   7 15  1 0.327 0.373 0.539 0.913 0.387    0.418
#> 81   20 29   1  0  4   9  0  0 0.325 0.377 0.528 0.906 0.385    0.417
#> 82   17 68   2  0  1   5  1  0 0.278 0.351 0.566 0.917 0.385    0.545
#> 83   12 32   0  0  1   1  1  0 0.302 0.370 0.538 0.907 0.385    0.508
#> 84   34 87   1  0  1   3  1  0 0.259 0.370 0.532 0.902 0.385    0.590
#> 85    6 26   4  0  0   3  0  0 0.320 0.381 0.505 0.885 0.384    0.472
#> 86    7 21  11  0  0   1  0  2 0.297 0.384 0.500 0.884 0.384    0.406
#> 87    2 21   4  1  0   3  0  2 0.338 0.390 0.493 0.883 0.384    0.507
#> 88   19 49   2  0  1   1  4  1 0.294 0.374 0.513 0.886 0.381    0.495
#> 89   19 62   0  0  2   2  5  2 0.259 0.324 0.584 0.908 0.381    0.519
#> 90    2  8   1  2  1   0  0  0 0.381 0.440 0.429 0.869 0.381    0.571
#> 91   16 26   1  0  1   3  0  0 0.282 0.380 0.509 0.889 0.381    0.441
#> 92    6 22   1  0  0   1  0  0 0.288 0.365 0.530 0.895 0.381    0.522
#> 93   17 37   0  0  1  14  0  1 0.329 0.379 0.504 0.883 0.379    0.428
#> 94    9 15   0  0  0   1  0  0 0.328 0.416 0.463 0.878 0.379    0.436
#> 95   12 30   2  1  0   4  1  0 0.302 0.366 0.511 0.877 0.378    0.441
#> 96    0  2   0  0  0   0  0  0 0.429 0.429 0.429 0.857 0.378    0.529
#> 97   12 26   0  0  0   3  1  0 0.294 0.359 0.521 0.880 0.377    0.443
#> 98   22 27   1  0  1   7  0  0 0.316 0.410 0.454 0.864 0.376    0.403
#> 99    1  8   0  0  1   0  0  1 0.353 0.361 0.529 0.891 0.376    0.494
#> 100  39 62   1  0  3   3  1  3 0.266 0.387 0.493 0.880 0.376    0.458
#> 101  17 39   1  0  1   9  0  1 0.283 0.356 0.533 0.890 0.376    0.443
#> 102  14 33   1  0  5   6  3  0 0.319 0.363 0.524 0.887 0.375    0.436
#> 103   0  5   0  0  0   1  0  0 0.367 0.367 0.500 0.867 0.375    0.450
#> 104  38 53   4  0  2   1  2  1 0.258 0.384 0.470 0.854 0.373    0.422
#> 105   9 28   4  1  3   6 11  4 0.332 0.364 0.508 0.872 0.372    0.406
#> 106   3 21   0  0  0   1  0  0 0.284 0.312 0.568 0.879 0.372    0.502
#> 107  15 49   1  0  1   5  2  1 0.305 0.361 0.510 0.871 0.371    0.459
#> 108  17 33   0  0  2   3  0  0 0.286 0.347 0.527 0.874 0.370    0.421
#> 109  21 54   1  1  0   2  1  0 0.252 0.341 0.521 0.862 0.370    0.489
#> 110  24 48   9  0  2   2  3  1 0.263 0.367 0.498 0.864 0.370    0.417
#> 111  19 51   1  0  1   1  0  0 0.255 0.339 0.523 0.862 0.369    0.494
#> 112  28 30   1  0  1   3  2  1 0.295 0.378 0.473 0.851 0.369    0.380
#> 113  22 30   0  0  4   8  0  0 0.286 0.354 0.513 0.867 0.368    0.401
#> 114  14 33   0  0  2   4  0  1 0.276 0.333 0.533 0.866 0.368    0.438
#> 115  16 18   1  1  0   2  0  0 0.253 0.370 0.473 0.843 0.368    0.384
#> 116   0  6   0  0  0   0  1  0 0.345 0.345 0.517 0.862 0.367    0.463
#> 117  22 55   5  2  2   1  9  4 0.322 0.392 0.448 0.840 0.367    0.436
#> 118  27 26   0  0  0   2  8  1 0.302 0.388 0.448 0.836 0.367    0.372
#> 119  19 29   0  0  0   3  0  0 0.284 0.385 0.448 0.833 0.367    0.419
#> 120  24 19   2  1  0   9  1  0 0.317 0.401 0.425 0.826 0.366    0.358
#> 121  28 67   2  0  2   7  4  1 0.266 0.373 0.493 0.866 0.366    0.481
#> 122   9 13   0  0  4   4  1  0 0.303 0.327 0.544 0.871 0.366    0.385
#> 123   3 10   0  0  0   0  0  0 0.257 0.333 0.543 0.876 0.366    0.474
#> 124  17 48   0  0  1   4  0  0 0.252 0.342 0.519 0.860 0.365    0.507
#> 125   1  6   0  0  0   0  0  0 0.333 0.375 0.467 0.842 0.365    0.573
#> 126   5  4   1  0  0   0  0  1 0.167 0.375 0.444 0.819 0.364    0.327
#> 127  14 31   2  3  0   1  2  2 0.294 0.373 0.460 0.834 0.363    0.426
#> 128  17 45   0  0  1   1  0  0 0.285 0.351 0.494 0.845 0.363    0.451
#> 129   3  9   2  0  1   1  0  0 0.297 0.386 0.459 0.846 0.362    0.431
#> 130   1  4   1  0  0   1  0  1 0.333 0.391 0.429 0.820 0.362    0.407
#> 131   2  4   0  0  0   0  0  0 0.333 0.412 0.400 0.812 0.362    0.435
#> 132  20 28   1  0  1   5  1  1 0.277 0.379 0.454 0.833 0.362    0.398
#> 133  25 42   2  0  1   5  6  4 0.278 0.353 0.485 0.839 0.362    0.396
#> 134  18 43   1  0  0   3  2  0 0.284 0.354 0.483 0.837 0.362    0.432
#> 135  26 55   2  0  1   5  2  4 0.255 0.356 0.489 0.846 0.362    0.445
#> 136  13 17   4  1  3  11  0  0 0.295 0.343 0.500 0.843 0.361    0.369
#> 137  10 34   1  1  0   1  3  0 0.313 0.367 0.461 0.828 0.361    0.453
#> 138  19 61   1  0  1   4  1  1 0.275 0.341 0.502 0.843 0.360    0.468
#> 139  16 56   2  0  2   1  0  1 0.275 0.333 0.512 0.845 0.360    0.459
#> 140   9 18   1  3  0   2  2  0 0.258 0.330 0.505 0.835 0.359    0.400
#> 141   0  9   0  0  0   0  0  0 0.320 0.320 0.520 0.840 0.359    0.561
#> 142  15 37   2  1  0   7  5  1 0.332 0.380 0.440 0.820 0.358    0.397
#> 143  28 28   5  0  1   2  0  0 0.254 0.402 0.396 0.798 0.358    0.352
#> 144  17 55   5  0  0   1  7  3 0.322 0.388 0.421 0.809 0.358    0.442
#> 145  14 48   0  0  4   7  3  1 0.286 0.326 0.517 0.843 0.358    0.448
#> 146  15 29   0  0  3   3  0  0 0.288 0.357 0.480 0.837 0.358    0.426
#> 147   2 11   1  0  0   0  2  1 0.233 0.324 0.533 0.857 0.358    0.512
#> 148  11 22   2  0  1   1  2  0 0.286 0.403 0.397 0.799 0.358    0.452
#> 149  11 56   5  0  1   2  0  0 0.281 0.341 0.506 0.847 0.357    0.440
#> 150  18 36   2  0  1   5  0  0 0.265 0.344 0.481 0.826 0.357    0.409
#> 151  11 28   2  0  0   1  0  1 0.268 0.366 0.464 0.830 0.357    0.440
#> 152   0  2   0  0  0   0  0  0 0.333 0.333 0.500 0.833 0.356    0.534
#> 153   0  3   0  0  0   0  0  0 0.333 0.333 0.500 0.833 0.356    0.712
#> 154  24 39   0  0  2   4  8  3 0.265 0.345 0.490 0.835 0.356    0.397
#> 155   5 24   2  0  0   0  0  0 0.233 0.324 0.517 0.840 0.356    0.528
#> 156  11 56   1  0  3   6  0  0 0.252 0.298 0.557 0.855 0.356    0.466
#> 157  12 28   1  2  1   2  3  3 0.308 0.367 0.451 0.818 0.356    0.413
#> 158   6 30   0  0  0   1  0  1 0.250 0.300 0.536 0.836 0.355    0.515
#> 159  11 21   2  0  0   4  0  0 0.279 0.395 0.397 0.792 0.355    0.420
#> 160   0  9   0  0  0   0  0  0 0.333 0.333 0.500 0.833 0.355    0.474
#> 161  12 14   0  2  1   0  3  0 0.262 0.418 0.357 0.775 0.354    0.402
#> 162  11 32   0  1  0   0  2  1 0.282 0.349 0.479 0.827 0.354    0.445
#> 163  13 23   3  0  1   2  0  0 0.275 0.371 0.450 0.821 0.353    0.385
#> 164  32 62   3  0  1   1  4  1 0.257 0.361 0.450 0.810 0.352    0.419
#> 165   0  2   0  0  0   0  0  0 0.231 0.231 0.615 0.846 0.352    0.416
#> 166  27 57   4  0  1   3  2  1 0.216 0.337 0.473 0.810 0.352    0.442
#> 167   3  0   0  0  0   0  0  0 0.267 0.389 0.400 0.789 0.352    0.285
#> 168  19 34   1  0  1   6  2  0 0.252 0.341 0.476 0.817 0.351    0.402
#> 169   4 10   1  1  0   0  4  1 0.305 0.359 0.441 0.800 0.351    0.387
#> 170  26 41   0  1  2   5  2  0 0.289 0.381 0.410 0.791 0.349    0.399
#> 171  19 27   2  0  7   4  1  0 0.311 0.375 0.440 0.815 0.348    0.376
#> 172  20 41   3  0  2   4  5  3 0.266 0.333 0.476 0.809 0.348    0.385
#> 173  10 52   0  0  1  13  0  0 0.270 0.307 0.510 0.817 0.347    0.448
#> 174  11 27   1  0  1   3  1  0 0.267 0.356 0.444 0.800 0.347    0.436
#> 175   7 22   4  1  1   0  2  1 0.294 0.355 0.440 0.796 0.347    0.394
#> 176  16 41   5  1  2   3  6  1 0.235 0.335 0.470 0.805 0.346    0.430
#> 177  22 36   1  0  0  10  0  0 0.272 0.355 0.433 0.788 0.346    0.378
#> 178   4 16   0  0  0   3  1  0 0.303 0.333 0.472 0.805 0.346    0.404
#> 179  10 41   2  0  1   5  3  1 0.275 0.320 0.487 0.807 0.345    0.415
#> 180  15 45   3  0  4   4  2  1 0.284 0.346 0.463 0.809 0.345    0.418
#> 181   8 36   4  0  0   5  0  0 0.265 0.321 0.476 0.797 0.345    0.419
#> 182  13 24   2  0  0   1  2  1 0.266 0.345 0.444 0.789 0.345    0.376
#> 183  15 43   4  0  1   4  2  1 0.275 0.335 0.459 0.794 0.344    0.395
#> 184   4 11   1  3  0   1  1  0 0.304 0.336 0.461 0.797 0.344    0.366
#> 185   3 21   1  1  0   0  0  0 0.291 0.330 0.477 0.806 0.344    0.433
#> 186  15 38   1  0  1   2  1  1 0.270 0.354 0.437 0.791 0.344    0.433
#> 187   8 35   1  0  0   5  1  2 0.283 0.321 0.478 0.799 0.344    0.416
#> 188  18 45   4  0  2   1  2  0 0.237 0.352 0.432 0.784 0.344    0.461
#> 189  17 23   0  1  2   3 11  3 0.313 0.360 0.429 0.789 0.343    0.357
#> 190  18 33   0  0  0   4  0  0 0.286 0.378 0.429 0.807 0.343    0.382
#> 191   8 18   1  0  3   2  3  1 0.308 0.332 0.467 0.799 0.342    0.363
#> 192  13 19   2  0  2   6  0  0 0.306 0.367 0.437 0.804 0.342    0.353
#> 193  10 28   0  0  1   4  7  4 0.302 0.337 0.453 0.789 0.341    0.384
#> 194  12 54   0  0  2   6  0  1 0.284 0.322 0.474 0.796 0.340    0.447
#> 195  15 37   1  0  2   6  0  0 0.250 0.307 0.495 0.802 0.340    0.388
#> 196   6 16   0  0  0   0  2  1 0.267 0.321 0.467 0.788 0.340    0.397
#> 197  19 15   0  1  0   3  1  3 0.259 0.339 0.443 0.782 0.340    0.330
#> 198  15 40   2  0  0   3  0  1 0.256 0.357 0.421 0.779 0.339    0.432
#> 199  10 24   2  0  2   7  9  2 0.319 0.350 0.429 0.779 0.339    0.362
#> 200   6 12   3  0  0  10  0  0 0.310 0.355 0.423 0.778 0.339    0.346
#> 201   5 15   1  0  0   6  0  1 0.292 0.326 0.460 0.786 0.338    0.362
#> 202   6 27   1  4  0   1  1  2 0.286 0.325 0.454 0.779 0.338    0.410
#> 203  13 30   2  0  0   3  0  2 0.245 0.341 0.436 0.778 0.338    0.399
#> 204  10 37   5  0  1   4  9  2 0.294 0.344 0.431 0.775 0.337    0.381
#> 205   8 27   0  0  0   1 10  2 0.294 0.344 0.437 0.781 0.337    0.406
#> 206   5  6   1  0  1   1  1  0 0.283 0.343 0.433 0.777 0.337    0.341
#> 207  12 27   1  2  3   1 24  7 0.331 0.361 0.411 0.772 0.337    0.363
#> 208  12 26   3  2  1   2  0  0 0.286 0.372 0.381 0.753 0.336    0.384
#> 209  24 37   1  0  1   5  3  0 0.268 0.347 0.427 0.774 0.336    0.358
#> 210   1 11   0  0  0   5  1  0 0.333 0.341 0.432 0.774 0.336    0.384
#> 211  16 32   0  0  2   4  6  0 0.263 0.321 0.457 0.778 0.334    0.373
#> 212  12 38   3  2  0   2 15  3 0.283 0.331 0.444 0.774 0.334    0.374
#> 213   5 15   0  1  0   3  2  1 0.307 0.350 0.413 0.763 0.334    0.388
#> 214  22 67   0  2  2   5  6  2 0.268 0.335 0.431 0.765 0.333    0.441
#> 215  22 43   0  2  2   1  7  4 0.288 0.352 0.410 0.762 0.333    0.376
#> 216   5 57   3  1  2   8  1  0 0.288 0.311 0.465 0.776 0.333    0.439
#> 217  21 35   2  1  1   6  0  0 0.283 0.365 0.382 0.747 0.332    0.359
#> 218  13 40   1  0  0   3  5  4 0.299 0.351 0.402 0.753 0.331    0.392
#> 219  10 31   6  0  2   1  1  0 0.257 0.324 0.441 0.764 0.331    0.372
#> 220  16 21   1  0  1   5  0  0 0.303 0.368 0.399 0.767 0.330    0.336
#> 221   5 18   1  0  1   0  0  0 0.267 0.328 0.433 0.762 0.330    0.428
#> 222   6 35   5  1  2   2  1  4 0.262 0.305 0.477 0.782 0.330    0.381
#> 223  10 24   3  0  0   2  0  0 0.209 0.289 0.478 0.767 0.330    0.365
#> 224   4 15   2  1  0   2  0  0 0.200 0.294 0.467 0.761 0.329    0.420
#> 225  11 20   1  1  0   1  2  2 0.236 0.317 0.445 0.763 0.329    0.354
#> 226   6 12   0  0  0   0  0  0 0.279 0.338 0.412 0.750 0.329    0.361
#> 227  26 53   1  0  2   5  0  1 0.231 0.315 0.443 0.759 0.329    0.381
#> 228   4 15   2  0  1   1  1  0 0.287 0.330 0.425 0.755 0.328    0.370
#> 229   5 36   2  0  2   0  4  3 0.252 0.288 0.488 0.776 0.328    0.442
#> 230  21 38   3  0  2  10  0  0 0.268 0.344 0.411 0.755 0.328    0.351
#> 231  11 41   4  0  2   0  0  0 0.268 0.323 0.437 0.761 0.328    0.389
#> 232   0 23   0  0  1   0  0  2 0.273 0.270 0.511 0.781 0.328    0.449
#> 233  15 25   1  0  2  10  0  0 0.259 0.319 0.441 0.760 0.328    0.350
#> 234  10 35   1  0  1   2  0  0 0.226 0.282 0.489 0.771 0.328    0.405
#> 235  15 32   2  0  6   2  1  0 0.263 0.328 0.451 0.780 0.328    0.372
#> 236   1  2   0  2  0   0  0  0 0.273 0.304 0.455 0.759 0.327    0.342
#> 237  12 15   1  0  0   1  6  1 0.274 0.354 0.389 0.744 0.327    0.329
#> 238   7 33   9  1  2   6 10  2 0.277 0.328 0.427 0.755 0.327    0.357
#> 239  14 20   0  0  1   3  1  0 0.280 0.350 0.392 0.742 0.327    0.345
#> 240  34 48   1  0  2   8  4  2 0.239 0.357 0.394 0.751 0.327    0.350
#> 241   7 15   2  1  0   2  0  0 0.235 0.350 0.373 0.723 0.326    0.370
#> 242  20 25   0  1  0   2  5  1 0.246 0.327 0.423 0.749 0.326    0.333
#> 243   9 27   2  0  1   6  0  0 0.242 0.317 0.452 0.768 0.326    0.378
#> 244   7  6   0  0  0   1  3  1 0.262 0.367 0.357 0.724 0.326    0.310
#> 245  10 52   2  2  2   4  1  0 0.271 0.306 0.452 0.759 0.326    0.404
#> 246  14 21   1  0  0   2  0  0 0.277 0.377 0.366 0.743 0.326    0.342
#> 247  16 23   0  0  0   4  4  4 0.280 0.333 0.412 0.746 0.325    0.334
#> 248  14 43   7  0  3   2  0  2 0.247 0.333 0.411 0.745 0.324    0.385
#> 249  12 52   1  0  4   4  0  0 0.266 0.306 0.459 0.765 0.324    0.398
#> 250  17 24   0  0  0   2  2  0 0.208 0.351 0.364 0.715 0.324    0.354
#> 251  11 32   2  0  1   3  1  0 0.223 0.296 0.455 0.751 0.324    0.391
#> 252   1  1   0  1  0   0  0  0 0.200 0.333 0.400 0.733 0.324    0.314
#> 253  10 44   4  0  0   2  1  0 0.235 0.309 0.443 0.752 0.323    0.408
#> 254   8 22   2  0  0   1  1  2 0.294 0.353 0.376 0.729 0.323    0.362
#> 255   2 13   1  0  0   1  0  1 0.222 0.300 0.444 0.744 0.323    0.542
#> 256  13 28   1  3  3   1  7  1 0.318 0.354 0.381 0.735 0.323    0.348
#> 257   8 23   0  0  0   1  0  0 0.242 0.324 0.409 0.733 0.322    0.426
#> 258   9 50   1  0  0   2  4  1 0.283 0.322 0.416 0.739 0.322    0.422
#> 259   9 24   3  1  2   4 10  1 0.287 0.327 0.416 0.743 0.322    0.344
#> 260  13 29   0  0  1   2  0  1 0.248 0.321 0.424 0.745 0.322    0.372
#> 261   8 27   0  0  1   1  0  0 0.233 0.329 0.417 0.745 0.321    0.504
#> 262  16 45   0  1  0   3  9  3 0.276 0.325 0.414 0.739 0.321    0.367
#> 263  11 26   2  0  0   1  0  0 0.245 0.333 0.388 0.721 0.321    0.370
#> 264  15 33   0  0  1   2  0  0 0.264 0.338 0.388 0.726 0.321    0.377
#> 265   6 13   0  0  1   4  2  0 0.298 0.333 0.404 0.737 0.320    0.345
#> 266   1  7   0  0  0   0  0  0 0.227 0.261 0.500 0.761 0.320    0.445
#> 267  14 56   1  1  2   4  2  0 0.250 0.312 0.439 0.750 0.320    0.426
#> 268   1  0   0  0  0   0  0  0 0.222 0.300 0.444 0.744 0.320    0.279
#> 269   3 23   0  0  0   0  1  2 0.293 0.329 0.413 0.742 0.319    0.439
#> 270  11 34   2  1  1   2  5  4 0.286 0.332 0.402 0.734 0.319    0.357
#> 271  14 36   0  0  1   0  3  0 0.276 0.346 0.386 0.732 0.319    0.380
#> 272  13 22   1  1  1   3  2  2 0.247 0.349 0.376 0.725 0.319    0.349
#> 273  11 47   2  0  4   4  0  0 0.261 0.298 0.445 0.744 0.319    0.389
#> 274   8 33   0  0  0   3  1  0 0.263 0.317 0.421 0.738 0.319    0.413
#> 275   6 12   0  0  1   0  0  0 0.205 0.294 0.455 0.749 0.319    0.379
#> 276  13 42   3  0  1   1  1  1 0.269 0.337 0.394 0.731 0.319    0.385
#> 277  24 50   3  0  2   3  1  0 0.263 0.341 0.378 0.719 0.319    0.359
#> 278  15 22   0  0  0   3  0  0 0.207 0.370 0.310 0.680 0.318    0.358
#> 279   3 26   0  0  2   8  0  0 0.277 0.288 0.458 0.746 0.317    0.378
#> 280   2  6   0  0  0   0  0  0 0.250 0.300 0.429 0.729 0.316    0.368
#> 281   1  4   1  0  0   0  0  0 0.250 0.400 0.333 0.733 0.316    0.377
#> 282   7 35   3  2  1   0  9  2 0.240 0.306 0.420 0.726 0.316    0.432
#> 283  28 45   0  1  1   8  1  0 0.253 0.343 0.373 0.716 0.316    0.340
#> 284  10 32   3  0  1   2  0  1 0.194 0.314 0.403 0.717 0.316    0.455
#> 285   9 47   0  1  0   4  2  0 0.266 0.311 0.416 0.727 0.314    0.420
#> 286   1  2   0  0  0   0  0  0 0.250 0.400 0.250 0.650 0.314    0.440
#> 287  23 33   5  0  2   2  0  0 0.198 0.313 0.401 0.714 0.314    0.317
#> 288   0  3   0  0  0   0  0  0 0.250 0.250 0.500 0.750 0.314    0.502
#> 289  27 30   2  0  2   7  0  0 0.195 0.349 0.339 0.688 0.314    0.305
#> 290   0  1   0  0  0   0  0  0 0.250 0.250 0.500 0.750 0.314    0.419
#> 291   0  2   0  0  0   0  0  0 0.250 0.250 0.500 0.750 0.314    0.628
#> 292  14 33   5  0  3   1  1  1 0.272 0.330 0.381 0.712 0.313    0.337
#> 293   4 12   0  1  1   3  0  0 0.267 0.320 0.400 0.720 0.313    0.390
#> 294  15 35   0  0  1   9  8  0 0.286 0.326 0.387 0.713 0.312    0.338
#> 295   8 12   1  1  0   2  0  0 0.260 0.330 0.385 0.716 0.312    0.316
#> 296   1 12   0  1  0   1  0  0 0.222 0.250 0.481 0.731 0.311    0.535
#> 297   2 19   0  0  2   0  0  0 0.262 0.275 0.462 0.737 0.311    0.437
#> 298   7 48   1  0  2   1  1  0 0.261 0.296 0.430 0.726 0.311    0.445
#> 299   1 10   0  0  0   0  0  0 0.235 0.257 0.471 0.728 0.310    0.424
#> 300  12 19   3  0  0   1  1  0 0.233 0.364 0.301 0.665 0.310    0.313
#> 301   8 10   0  0  1   1  0  0 0.277 0.330 0.372 0.702 0.309    0.314
#> 302   6 17   1  0  1   6  0  0 0.302 0.335 0.378 0.713 0.309    0.327
#> 303   6 24   2  0  2   5  4  1 0.271 0.307 0.413 0.720 0.309    0.347
#> 304   3 21   0  0  0   1  2  0 0.259 0.295 0.414 0.709 0.308    0.452
#> 305  17 25   4  1  3   3  4  1 0.237 0.340 0.351 0.691 0.308    0.312
#> 306  26 65   3  0  0   5  9  5 0.232 0.332 0.351 0.682 0.307    0.375
#> 307   5 11   2  0  1   2  0  0 0.250 0.310 0.395 0.704 0.307    0.322
#> 308   3 28   3  0  2   8  0  0 0.276 0.307 0.414 0.722 0.307    0.351
#> 309  11 31   1  0  0   2  0  0 0.211 0.291 0.421 0.712 0.307    0.367
#> 310  17 18   1  1  3   7 14  3 0.264 0.318 0.382 0.700 0.306    0.303
#> 311   1  2   0  1  0   0  0  0 0.286 0.375 0.286 0.661 0.306    0.352
#> 312   3 10   0  0  0   2  0  0 0.267 0.300 0.419 0.719 0.306    0.331
#> 313   1  0   0  0  0   0  1  0 0.286 0.375 0.286 0.661 0.306    0.252
#> 314   4 14   0  0  0   5  3  0 0.227 0.271 0.439 0.711 0.306    0.360
#> 315   4  9   0  0  0   0  2  2 0.250 0.325 0.361 0.686 0.306    0.351
#> 316   1  4   0  1  0   0  0  0 0.125 0.222 0.500 0.722 0.306    0.516
#> 317   5 12   1  3  0   0  0  0 0.225 0.326 0.350 0.676 0.305    0.352
#> 318  16 68   3  0  1   2  0  0 0.226 0.282 0.423 0.704 0.305    0.386
#> 319   7 49   5  0  3   5  0  0 0.237 0.282 0.428 0.710 0.305    0.394
#> 320   8 24   1  0  1   3  0  0 0.259 0.330 0.358 0.688 0.305    0.378
#> 321   9 29   1  1  0   1  1  1 0.263 0.313 0.380 0.692 0.304    0.350
#> 322  18 47   0  0  2   4  0  1 0.256 0.330 0.366 0.696 0.304    0.368
#> 323   6  9   0  0  0   1  2  0 0.278 0.323 0.367 0.690 0.304    0.309
#> 324  16 37   2  0  1   7  0  1 0.191 0.291 0.405 0.696 0.303    0.351
#> 325  12 31   1  0  1   2  6  1 0.250 0.313 0.375 0.688 0.303    0.348
#> 326   7 38   3  0  0  11  2  1 0.232 0.300 0.419 0.719 0.302    0.367
#> 327  12 55   2  0  3   2  0  1 0.237 0.284 0.419 0.703 0.302    0.386
#> 328  19 53   3  0  2   8  0  1 0.223 0.294 0.398 0.692 0.302    0.353
#> 329   2  3   0  0  0   1  0  0 0.265 0.306 0.382 0.688 0.302    0.306
#> 330  14 27   0  1  3   8  2  0 0.259 0.303 0.397 0.699 0.301    0.318
#> 331   8 21   0  5  0   3  7  3 0.271 0.303 0.390 0.693 0.301    0.322
#> 332  20 44   2  0  2   7  0  0 0.235 0.314 0.370 0.684 0.300    0.333
#> 333   4 31   0  0  0   2  0  0 0.283 0.309 0.377 0.686 0.300    0.404
#> 334   4 60   0  1  2   2  3  1 0.241 0.262 0.462 0.725 0.300    0.412
#> 335   6 24   0  0  1   4  1  0 0.271 0.307 0.383 0.690 0.300    0.363
#> 336   2  8   0  0  1   0  0  0 0.261 0.308 0.391 0.699 0.299    0.427
#> 337   7 30   2  3  1   1 10  2 0.250 0.297 0.396 0.693 0.299    0.349
#> 338   3 19   0  0  1   2  0  1 0.270 0.283 0.412 0.695 0.299    0.337
#> 339   5 30   1  0  1   5  0  1 0.207 0.258 0.439 0.697 0.298    0.431
#> 340   2 10   0  2  1   1  8  2 0.271 0.290 0.407 0.697 0.298    0.349
#> 341  16 31   0  0  0   2  4  1 0.248 0.316 0.354 0.670 0.298    0.321
#> 342   3  6   0  0  0   0  0  0 0.190 0.292 0.381 0.673 0.298    0.339
#> 343  13 26   1  0  0   2  2  1 0.248 0.310 0.363 0.673 0.297    0.315
#> 344   3 12   0  1  0   3  0  0 0.268 0.294 0.390 0.684 0.297    0.332
#> 345   3 28   0  0  0   4  1  2 0.286 0.298 0.387 0.685 0.297    0.348
#> 346   3 18   2  0  1   2  0  0 0.271 0.316 0.357 0.673 0.297    0.366
#> 347  23 51   4  0  0   5  0  0 0.236 0.326 0.330 0.656 0.297    0.326
#> 348   9 21   0  0  5   3  2  1 0.259 0.302 0.393 0.694 0.297    0.344
#> 349  14 31   1  1  2   3  3  0 0.266 0.323 0.349 0.672 0.297    0.325
#> 350   9 35   1  1  1   5  3  5 0.263 0.300 0.380 0.680 0.296    0.343
#> 351   8 28   2  0  2   0  1  1 0.247 0.321 0.355 0.676 0.296    0.371
#> 352   8 30   1  0  4   5  2  0 0.263 0.294 0.395 0.690 0.296    0.343
#> 353  24 59   0  0  1   8  3  0 0.194 0.298 0.383 0.681 0.296    0.365
#> 354   9 22   0  2  0   1  0  0 0.224 0.291 0.393 0.683 0.295    0.331
#> 355   2  8   0  0  0   0  0  0 0.241 0.290 0.379 0.670 0.294    0.368
#> 356   0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667 0.294    0.440
#> 357   0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667 0.294    0.440
#> 358   0  0   0  0  0   0  0  0 0.333 0.333 0.333 0.667 0.294    0.294
#> 359   0  2   0  0  0   0  0  0 0.333 0.333 0.333 0.667 0.294    0.881
#> 360   0  2   0  0  0   0  0  0 0.333 0.333 0.333 0.667 0.294    0.881
#> 361   6 45   2  0  1   7  0  0 0.227 0.258 0.427 0.685 0.294    0.368
#> 362   0  1   0  1  0   0  0  0 0.333 0.333 0.333 0.667 0.294    0.352
#> 363   0  0   0  0  0   1  0  0 0.333 0.333 0.333 0.667 0.294    0.294
#> 364  14 34   0  0  1   3  6  2 0.259 0.316 0.346 0.662 0.294    0.331
#> 365   0  2   0  0  0   0  1  0 0.333 0.333 0.333 0.667 0.294    0.440
#> 366   4 24   1  1  3   0  0  0 0.288 0.333 0.348 0.682 0.294    0.436
#> 367  14 47   3  0  3   6  2  1 0.243 0.303 0.374 0.676 0.293    0.343
#> 368   3 12   0  0  0   0  0  0 0.185 0.290 0.407 0.698 0.293    0.448
#> 369  17 36   1  0  0   2  4  0 0.221 0.291 0.370 0.662 0.292    0.315
#> 370   6 13   0  2  0   0  0  0 0.204 0.291 0.367 0.658 0.292    0.331
#> 371  14 33   2  1  2   8  0  0 0.211 0.308 0.366 0.674 0.292    0.334
#> 372  12 22   1  0  1   2  0  0 0.210 0.323 0.333 0.656 0.292    0.319
#> 373   8 38   1  0  3   9  0  0 0.223 0.260 0.420 0.681 0.291    0.360
#> 374   1  6   3  1  0   0  2  1 0.267 0.353 0.267 0.620 0.291    0.294
#> 375  15 24   2  0  2   8  3  1 0.226 0.292 0.387 0.679 0.291    0.294
#> 376   7 12   0  1  1   4  0  0 0.269 0.316 0.352 0.668 0.291    0.302
#> 377   5 24   0  2  1   2  0  0 0.237 0.263 0.420 0.683 0.290    0.340
#> 378   7 65   2  0  3   5  1  0 0.242 0.273 0.411 0.683 0.290    0.404
#> 379  21 48   2  3  4   1  9  5 0.239 0.310 0.346 0.657 0.290    0.327
#> 380  13 39   4  0  1   4  1  0 0.226 0.309 0.342 0.652 0.290    0.333
#> 381   3 15   0  1  1   4  0  0 0.236 0.263 0.417 0.680 0.289    0.350
#> 382   4  7   0  2  0   1  1  0 0.261 0.320 0.326 0.646 0.289    0.300
#> 383   9 27   3  3  0   4  0  0 0.258 0.311 0.335 0.647 0.289    0.311
#> 384   7  8   1  0  0   1  2  0 0.203 0.276 0.380 0.656 0.288    0.274
#> 385   5 22   1  1  0   3  0  0 0.226 0.264 0.400 0.664 0.288    0.329
#> 386   1  6   0  0  0   1  1  0 0.282 0.300 0.359 0.659 0.288    0.328
#> 387  14 14   4  2  0   1  3  2 0.198 0.330 0.286 0.616 0.287    0.244
#> 388   1  2   0  0  0   0  0  1 0.200 0.273 0.400 0.673 0.287    0.309
#> 389  30 43   1  1  1   2  2  1 0.167 0.338 0.278 0.615 0.287    0.289
#> 390   9 21   1  3  1   2  1  0 0.239 0.313 0.330 0.643 0.287    0.321
#> 391   2  5   0  0  0   0  0  0 0.250 0.357 0.250 0.607 0.287    0.378
#> 392   6 29   1  1  1   1  1  0 0.197 0.266 0.394 0.660 0.286    0.423
#> 393  10 36   0  1  0   2  4  1 0.263 0.311 0.340 0.651 0.286    0.338
#> 394   6 38   0  0  1   3  0  0 0.222 0.261 0.413 0.674 0.286    0.386
#> 395   0  5   0  1  0   1  0  0 0.308 0.308 0.346 0.654 0.286    0.353
#> 396   2  3   0  0  0   0  0  0 0.222 0.364 0.222 0.586 0.285    0.294
#> 397   8 39   1  2  0   3  1  1 0.238 0.286 0.378 0.664 0.285    0.345
#> 398   3  8   1  0  1   0  0  0 0.083 0.294 0.333 0.627 0.285    0.516
#> 399   6 25   1  0  0   1  0  0 0.212 0.272 0.376 0.648 0.284    0.355
#> 400  15 40   0  2  2   7 10  0 0.258 0.313 0.328 0.642 0.282    0.318
#> 401   1  4   0  2  0   0  0  0 0.222 0.300 0.333 0.633 0.282    0.427
#> 402   9 20   0  0  2   4  4  0 0.279 0.320 0.327 0.647 0.282    0.300
#> 403   3  7   0  0  0   1  0  0 0.270 0.325 0.297 0.622 0.281    0.306
#> 404   4 35   0  0  1   4  0  0 0.246 0.268 0.381 0.650 0.281    0.383
#> 405   2 10   0  0  0   0  0  0 0.222 0.300 0.333 0.633 0.281    0.530
#> 406  15 42   0  2  0   4  3  1 0.231 0.300 0.341 0.641 0.281    0.324
#> 407  12 61   2  1  3   7  3  3 0.218 0.268 0.386 0.654 0.280    0.356
#> 408   2 11   0  4  0   0  0  0 0.250 0.318 0.300 0.618 0.280    0.531
#> 409  11 40   0  0  0   1  0  0 0.206 0.294 0.340 0.634 0.279    0.397
#> 410   9 24   0  0  0   3  0  0 0.162 0.262 0.378 0.640 0.278    0.337
#> 411   1  3   0  0  0   0  0  0 0.250 0.286 0.350 0.636 0.278    0.303
#> 412   5 14   1  0  1   2  0  0 0.190 0.244 0.405 0.649 0.278    0.304
#> 413   6  8   1  1  0   1  0  0 0.211 0.308 0.316 0.623 0.277    0.263
#> 414   4 15   0  0  0   2  0  0 0.188 0.278 0.344 0.622 0.277    0.425
#> 415  10 39   3  0  2   4  1  1 0.216 0.269 0.363 0.631 0.276    0.321
#> 416   0  3   0  0  1   0  0  0 0.286 0.278 0.371 0.649 0.276    0.310
#> 417   4 12   0  3  0   2  1  0 0.259 0.306 0.310 0.617 0.276    0.312
#> 418   6 14   0  0  0   1  1  0 0.250 0.302 0.313 0.615 0.275    0.295
#> 419   2  1   0  0  0   0  0  0 0.000 0.400 0.000 0.400 0.275    0.000
#> 420  13 27   0  0  0   1  0  2 0.167 0.278 0.333 0.612 0.275    0.311
#> 421  11 26   1  2  2   6  0  0 0.248 0.311 0.298 0.609 0.274    0.302
#> 422   2  8   1  0  0   5  0  0 0.214 0.254 0.375 0.629 0.274    0.293
#> 423   4 13   0  0  0   0  1  0 0.150 0.292 0.300 0.592 0.274    0.547
#> 424  14 37   2  0  2  11  0  1 0.249 0.311 0.307 0.618 0.274    0.300
#> 425   7 19   1  2  1   0  0  1 0.221 0.291 0.325 0.615 0.274    0.311
#> 426  10 25   0  0  1   1  0  1 0.239 0.297 0.316 0.613 0.273    0.305
#> 427   6 16   0  3  0   6  1  0 0.245 0.288 0.327 0.615 0.272    0.295
#> 428   3 27   1  2  0   1  1  0 0.227 0.261 0.364 0.625 0.272    0.365
#> 429   3  6   0  0  0   2  0  0 0.250 0.333 0.250 0.583 0.272    0.294
#> 430   2 14   0  0  0   0  2  1 0.256 0.289 0.326 0.614 0.272    0.375
#> 431  14 25   1  0  2   2  0  0 0.201 0.269 0.354 0.623 0.271    0.279
#> 432   7 29   0  0  0   2  0  1 0.211 0.277 0.329 0.606 0.271    0.375
#> 433  10 24   0  3  1   2  0  0 0.220 0.289 0.330 0.620 0.271    0.302
#> 434  12 48   3  1  1   4  7  2 0.232 0.283 0.332 0.615 0.270    0.310
#> 435   9 36   1  6  2   4  4  0 0.252 0.288 0.330 0.618 0.270    0.305
#> 436   1  8   0  4  0   0  0  0 0.250 0.294 0.313 0.607 0.270    0.487
#> 437  23 50   2  2  1   2  5  2 0.208 0.294 0.297 0.591 0.269    0.290
#> 438   5  8   0  0  2   0  0  1 0.179 0.324 0.321 0.646 0.269    0.298
#> 439   1 10   0  1  0   0  0  0 0.288 0.302 0.308 0.610 0.269    0.324
#> 440   6 28   2  0  1   2  2  0 0.221 0.286 0.326 0.612 0.269    0.334
#> 441   6 17   1  0  0   1  0  0 0.177 0.261 0.339 0.600 0.268    0.303
#> 442  20 53   1  0  2   7  3  1 0.206 0.272 0.339 0.611 0.268    0.301
#> 443   1  9   0  1  0   1  0  1 0.269 0.296 0.308 0.604 0.268    0.385
#> 444  13 32   1  4  0   4  2  1 0.217 0.278 0.325 0.603 0.268    0.288
#> 445   2 28   1  0  1   2  0  0 0.200 0.228 0.400 0.628 0.267    0.405
#> 446   8 19   0  0  4   4  0  0 0.233 0.268 0.358 0.626 0.267    0.284
#> 447  12 31   3  0  2   4  4  2 0.236 0.293 0.305 0.598 0.267    0.284
#> 448   2 11   1  1  0   0  0  1 0.250 0.304 0.308 0.611 0.266    0.306
#> 449   3  9   4  2  0   3  0  0 0.179 0.304 0.256 0.561 0.266    0.243
#> 450   6 34   2  3  1   1  5  1 0.232 0.261 0.355 0.615 0.266    0.298
#> 451   5 23   1  3  1   6  8  2 0.255 0.273 0.332 0.605 0.264    0.283
#> 452   7 18   0  2  0   2  0  0 0.194 0.306 0.306 0.612 0.263    0.303
#> 453   6 15   0  0  0   0  3  0 0.176 0.300 0.265 0.565 0.263    0.337
#> 454   3  1   0  1  0   1  1  0 0.154 0.313 0.231 0.543 0.262    0.178
#> 455   2 16   1  1  1   1  2  1 0.234 0.275 0.319 0.594 0.262    0.363
#> 456   2  4   1  1  0   0  0  0 0.000 0.375 0.000 0.375 0.262    0.000
#> 457  11 49   1  1  0   1  3  1 0.224 0.276 0.322 0.598 0.262    0.320
#> 458   6  9   1  1  0   1  1  2 0.248 0.301 0.286 0.587 0.261    0.254
#> 459   0  6   0  1  0   1  0  0 0.231 0.231 0.385 0.615 0.261    0.485
#> 460  10 36   0  0  1   1  0  0 0.216 0.279 0.306 0.585 0.260    0.331
#> 461   7 16   0  0  1   2  8  1 0.222 0.288 0.292 0.579 0.260    0.286
#> 462  11 60   1  0  0   4  4  0 0.209 0.273 0.330 0.602 0.259    0.344
#> 463   3  8   0  0  1   0  1  1 0.231 0.279 0.308 0.587 0.258    0.292
#> 464   4 40   1  2  1   5 10  3 0.243 0.259 0.335 0.594 0.258    0.305
#> 465  11 27   2  2  0   1  8  0 0.214 0.314 0.252 0.566 0.258    0.275
#> 466   5 37   2  1  0   5  0  1 0.235 0.266 0.319 0.585 0.257    0.307
#> 467   7 24   2  1  1   2  0  0 0.205 0.254 0.333 0.587 0.257    0.280
#> 468   0  4   1  0  0   0  0  0 0.200 0.238 0.350 0.588 0.256    0.291
#> 469   9 13   0  0  5   3  2  0 0.228 0.272 0.324 0.595 0.256    0.262
#> 470   6 30   0  0  1   0  0  0 0.202 0.245 0.343 0.589 0.256    0.333
#> 471  12 23   1  3  2   2  0  0 0.172 0.275 0.287 0.562 0.255    0.267
#> 472   5 20   0  0  0   0  0  0 0.175 0.242 0.333 0.575 0.255    0.334
#> 473  18 36   0  1  2   3  6  2 0.218 0.302 0.246 0.549 0.254    0.271
#> 474   8 38   0  0  4   6  0  0 0.216 0.249 0.341 0.590 0.253    0.305
#> 475   0  1   0  0  0   0  0  0 0.286 0.286 0.286 0.571 0.252    0.294
#> 476   1 30   0  0  0   2  0  0 0.204 0.212 0.378 0.590 0.252    0.356
#> 477   0  3   0  0  0   0  0  0 0.200 0.200 0.400 0.600 0.251    0.628
#> 478   0 30   0  0  1   2  2  0 0.260 0.258 0.323 0.581 0.250    0.330
#> 479   4 16   3  2  2   1  2  0 0.213 0.282 0.280 0.562 0.248    0.269
#> 480   1 20   0  0  0   0  0  0 0.194 0.216 0.361 0.577 0.247    0.527
#> 481   2  6   0  2  0   0  0  0 0.240 0.296 0.240 0.536 0.247    0.278
#> 482   3  6   0  0  0   0  0  0 0.154 0.241 0.308 0.549 0.246    0.254
#> 483   6 23   1  0  1   3  0  1 0.167 0.238 0.319 0.557 0.246    0.302
#> 484   1 15   1  0  0   1  0  0 0.178 0.213 0.356 0.568 0.246    0.339
#> 485   1 13   0  1  0   0  0  0 0.167 0.200 0.375 0.575 0.246    0.496
#> 486   4 22   0  2  0   1  2  1 0.209 0.244 0.314 0.558 0.244    0.300
#> 487  11 31   1  1  0   2  0  0 0.180 0.254 0.287 0.541 0.244    0.269
#> 488   5 19   0  1  0   1  1  1 0.174 0.255 0.283 0.538 0.243    0.333
#> 489   3  2   0  0  0   0  2  1 0.200 0.304 0.200 0.504 0.243    0.196
#> 490  12 15   0  3  2   3  2  0 0.214 0.266 0.277 0.543 0.242    0.233
#> 491   9 25   0  0  0   5  0  0 0.171 0.237 0.305 0.542 0.241    0.266
#> 492   6 34   2  1  0   8  7  0 0.208 0.246 0.302 0.547 0.241    0.278
#> 493   2 10   0  2  0   0  0  0 0.227 0.292 0.227 0.519 0.241    0.367
#> 494  10 58   0  0  4   3  3  0 0.175 0.216 0.355 0.572 0.240    0.301
#> 495   6 13   0  0  0   0  0  0 0.139 0.279 0.250 0.529 0.240    0.259
#> 496   0  9   0  1  0   0  0  0 0.250 0.250 0.300 0.550 0.239    0.435
#> 497   3 20   2  0  0   0  0  0 0.169 0.220 0.325 0.544 0.239    0.283
#> 498   4 34   0  0  0   5  0  0 0.200 0.228 0.318 0.546 0.238    0.321
#> 499   2 16   0  1  1   0  1  0 0.214 0.235 0.327 0.562 0.237    0.276
#> 500   9 14   1  1  0   1  3  1 0.178 0.260 0.256 0.516 0.237    0.221
#> 501   1  9   1  0  0   0  0  0 0.182 0.229 0.303 0.532 0.236    0.285
#> 502   2 15   1  0  0   2  1  0 0.220 0.264 0.260 0.524 0.236    0.298
#> 503   4 20   3  2  0   3  4  0 0.213 0.271 0.236 0.507 0.233    0.253
#> 504   5 22   0  1  0   1  0  1 0.175 0.235 0.286 0.521 0.233    0.303
#> 505   2 15   0  0  0   2  0  1 0.204 0.232 0.296 0.528 0.232    0.298
#> 506   5 14   1  0  0   0  1  0 0.175 0.298 0.200 0.498 0.232    0.252
#> 507   8 30   3  6  2   3  8  0 0.216 0.251 0.270 0.521 0.231    0.243
#> 508   1  2   0  1  0   0  3  0 0.182 0.250 0.318 0.568 0.230    0.231
#> 509   1  0   0  0  0   0  0  0 0.000 0.333 0.000 0.333 0.229    0.000
#> 510   1  0   0  0  0   0  0  0 0.000 0.333 0.000 0.333 0.229    0.000
#> 511   6 37   0  0  0   2  1  0 0.171 0.216 0.305 0.521 0.229    0.312
#> 512   0  1   1  0  0   0  0  0 0.167 0.286 0.167 0.452 0.228    0.176
#> 513   3 18   3  1  0   3  2  0 0.183 0.239 0.268 0.507 0.228    0.248
#> 514   1  3   1  0  0   0  0  0 0.167 0.286 0.167 0.452 0.226    0.196
#> 515   1  7   0  0  0   0  0  1 0.176 0.222 0.294 0.516 0.225    0.336
#> 516   2  4   0  2  0   0  0  0 0.167 0.286 0.167 0.452 0.224    0.220
#> 517   3 26   0  0  0   2  1  0 0.203 0.239 0.266 0.504 0.224    0.341
#> 518   0  8   1  0  1   0  0  0 0.214 0.233 0.286 0.519 0.224    0.300
#> 519   3 10   0  0  0   1  0  0 0.171 0.237 0.257 0.494 0.223    0.256
#> 520   1  5   0  0  0   0  1  0 0.200 0.273 0.200 0.473 0.223    0.352
#> 521   4 14   0  0  2   3  0  0 0.191 0.245 0.255 0.501 0.223    0.274
#> 522   4 11   1  0  0   2  1  0 0.186 0.240 0.257 0.497 0.223    0.225
#> 523   3 11   0  1  2   1  2  1 0.200 0.240 0.267 0.507 0.223    0.268
#> 524   0  7   0  3  0   0  0  0 0.250 0.250 0.250 0.500 0.220    0.392
#> 525   0  3   0  0  0   0  1  0 0.250 0.250 0.250 0.500 0.220    0.352
#> 526   0  3   0  0  0   1  0  0 0.250 0.250 0.250 0.500 0.220    0.352
#> 527   0  3   0  0  0   0  0  0 0.250 0.250 0.250 0.500 0.220    0.881
#> 528  20 31   1  0  2   8  0  0 0.121 0.262 0.187 0.448 0.220    0.187
#> 529   0  2   0  0  0   0  0  0 0.250 0.250 0.250 0.500 0.220    0.440
#> 530   0  7   0  1  0   0  0  1 0.227 0.227 0.273 0.500 0.217    0.319
#> 531   4 14   0  0  0   2  0  0 0.152 0.222 0.288 0.510 0.215    0.237
#> 532   1 31   1  0  1   1  3  0 0.190 0.225 0.286 0.510 0.213    0.323
#> 533   4 18   0  0  0   1  0  0 0.129 0.229 0.226 0.454 0.213    0.362
#> 534   2 13   1  2  0   0  0  0 0.196 0.237 0.232 0.469 0.212    0.243
#> 535   6 13   0  5  0   6  0  0 0.180 0.232 0.236 0.468 0.212    0.210
#> 536   2  6   0  0  0   0  0  0 0.176 0.263 0.176 0.440 0.211    0.240
#> 537   2 11   0  1  1   1  1  0 0.170 0.187 0.307 0.494 0.209    0.229
#> 538   1  6   0  1  0   0  0  0 0.200 0.250 0.200 0.450 0.208    0.294
#> 539   0  8   0  0  0   2  0  0 0.136 0.136 0.364 0.500 0.208    0.327
#> 540   2 10   0  0  0   0  0  0 0.159 0.213 0.273 0.485 0.206    0.238
#> 541   1  8   0  0  0   2  0  1 0.143 0.182 0.286 0.468 0.205    0.294
#> 542   4 13   0  1  0   0  0  0 0.115 0.233 0.192 0.426 0.205    0.261
#> 543   1  3   0  1  0   2  0  0 0.182 0.250 0.182 0.432 0.204    0.220
#> 544   4 24   0  1  0   1  0  0 0.180 0.231 0.213 0.444 0.203    0.282
#> 545   0  7   0  2  0   0  0  0 0.231 0.231 0.231 0.462 0.203    0.278
#> 546   8 32   0  1  1   4  0  0 0.174 0.216 0.229 0.445 0.200    0.224
#> 547   1  6   0  0  0   1  0  0 0.105 0.150 0.316 0.466 0.200    0.255
#> 548   1 13   0  3  0   1  0  0 0.182 0.217 0.227 0.445 0.199    0.433
#> 549   2  6   0  0  0   0  1  0 0.077 0.200 0.231 0.431 0.198    0.228
#> 550   5 16   0  1  1   1  0  0 0.164 0.225 0.219 0.444 0.197    0.213
#> 551   2  8   0  0  1   1  0  0 0.133 0.182 0.267 0.448 0.196    0.231
#> 552   2  4   0  1  0   0  0  0 0.125 0.222 0.188 0.410 0.195    0.178
#> 553   4  8   0  0  0   2  0  0 0.095 0.240 0.143 0.383 0.195    0.164
#> 554   1 12   1  0  1   0  2  0 0.164 0.190 0.255 0.444 0.193    0.228
#> 555   4 16   0  0  0   0  0  0 0.169 0.217 0.200 0.417 0.191    0.213
#> 556   0  7   0  0  0   0  0  0 0.214 0.214 0.214 0.429 0.189    0.378
#> 557   1 16   1  0  0   0  0  0 0.128 0.171 0.256 0.427 0.189    0.276
#> 558   2  9   0  0  0   2  0  0 0.154 0.214 0.192 0.407 0.188    0.229
#> 559   0  6   0  1  0   0  0  0 0.167 0.167 0.278 0.444 0.188    0.283
#> 560   2 13   1  1  0   0  0  0 0.153 0.194 0.220 0.414 0.186    0.205
#> 561   2 10   0  0  0   0  0  0 0.150 0.227 0.150 0.377 0.183    0.264
#> 562   3 12   0  3  0   0  0  0 0.130 0.231 0.130 0.361 0.181    0.240
#> 563   0  1   1  0  0   0  3  0 0.000 0.250 0.000 0.250 0.180    0.000
#> 564   3 25   0  1  1   1  0  0 0.164 0.212 0.197 0.409 0.179    0.266
#> 565   0  7   0  0  0   0  0  1 0.176 0.176 0.235 0.412 0.178    0.302
#> 566   0  5   1  0  0   0  0  0 0.125 0.222 0.125 0.347 0.178    0.294
#> 567   0  4   0  2  0   0  0  0 0.200 0.200 0.200 0.400 0.176    0.881
#> 568   0  3   0  0  0   0  0  0 0.200 0.200 0.200 0.400 0.176    0.440
#> 569   0  1   0  0  1   0  0  0 0.250 0.200 0.250 0.450 0.176    0.294
#> 570   2 11   0  0  0   1  0  0 0.130 0.200 0.174 0.374 0.176    0.252
#> 571   3 11   0  0  1   2  0  0 0.143 0.242 0.143 0.385 0.175    0.207
#> 572   0 13   1  3  0   1  0  1 0.074 0.107 0.296 0.403 0.173    0.295
#> 573   0 12   0  0  0   4  0  0 0.184 0.184 0.211 0.395 0.172    0.252
#> 574   1  3   0  0  0   0  0  0 0.000 0.250 0.000 0.250 0.172      NaN
#> 575   0  9   0  0  0   0  0  0 0.136 0.136 0.273 0.409 0.171    0.290
#> 576   2 10   2  0  0   1  0  0 0.137 0.200 0.157 0.357 0.170    0.160
#> 577   1  8   0  1  0   2  0  0 0.167 0.200 0.167 0.367 0.168    0.220
#> 578   0  5   1  2  0   1  0  0 0.143 0.200 0.143 0.343 0.165    0.196
#> 579   2  9   0  2  0   0  1  0 0.118 0.211 0.118 0.328 0.165    0.220
#> 580   3 20   0  3  0   1  0  0 0.130 0.175 0.185 0.361 0.165    0.216
#> 581   0  4   1  0  0   0  0  0 0.167 0.186 0.167 0.353 0.160    0.162
#> 582   1  5   0  7  0   0  0  0 0.150 0.190 0.150 0.340 0.159    0.176
#> 583   1  1   0  0  0   0  1  0 0.111 0.200 0.111 0.311 0.157    0.110
#> 584   3  4   0  2  1   0  0  0 0.067 0.211 0.067 0.277 0.155    0.080
#> 585   0  5   0  1  0   0  0  0 0.174 0.174 0.174 0.348 0.153    0.196
#> 586   2  4   0  0  0   0  0  0 0.000 0.222 0.000 0.222 0.153    0.000
#> 587   2  5   0  0  0   0  1  0 0.105 0.190 0.105 0.296 0.149    0.126
#> 588   0  0   0  4  0   0  0  0 0.167 0.167 0.167 0.333 0.147    0.147
#> 589   0  4   0  0  0   0  0  0 0.167 0.167 0.167 0.333 0.147    0.440
#> 590   0  2   0  0  0   1  0  0 0.167 0.167 0.167 0.333 0.147    0.220
#> 591   0  1   0  1  0   0  0  0 0.167 0.167 0.167 0.333 0.147    0.176
#> 592   0  9   1  2  0   1  0  0 0.136 0.174 0.136 0.310 0.146    0.203
#> 593   2  8   0  5  0   0  0  0 0.063 0.167 0.125 0.292 0.146    0.157
#> 594   0  2   1  0  0   2  0  0 0.130 0.200 0.130 0.330 0.140    0.126
#> 595   0  8   0  0  0   2  0  0 0.129 0.129 0.194 0.323 0.138    0.186
#> 596   0  6   0  0  0   0  0  0 0.154 0.154 0.154 0.308 0.136    0.176
#> 597   0  8   1  0  0   0  0  0 0.100 0.143 0.150 0.293 0.136    0.178
#> 598   0 13   0  3  0   0  0  0 0.154 0.154 0.154 0.308 0.136    0.271
#> 599   0  4   0  2  0   1  0  0 0.150 0.150 0.150 0.300 0.132    0.165
#> 600   0  8   0  0  0   0  0  0 0.105 0.105 0.211 0.316 0.130    0.225
#> 601   0  7   0  2  0   0  0  0 0.143 0.143 0.143 0.286 0.126    0.252
#> 602   0  4   0  1  0   0  0  0 0.143 0.143 0.143 0.286 0.126    0.294
#> 603   0  9   0  2  0   0  0  0 0.143 0.143 0.143 0.286 0.126    0.352
#> 604   1  8   0  4  0   1  0  0 0.105 0.150 0.105 0.255 0.122    0.160
#> 605   0 11   0  0  0   0  0  0 0.059 0.059 0.235 0.294 0.121    0.344
#> 606   0  7   0  0  0   1  0  0 0.136 0.136 0.136 0.273 0.120    0.176
#> 607   0  3   0  3  0   0  0  0 0.133 0.133 0.133 0.267 0.117    0.147
#> 608   1  3   0  0  0   1  0  0 0.000 0.167 0.000 0.167 0.114    0.000
#> 609   1  1   0  0  0   0  0  0 0.000 0.167 0.000 0.167 0.114    0.000
#> 610   1  9   0  2  0   0  0  0 0.083 0.120 0.125 0.245 0.113    0.142
#> 611   2 11   0  1  0   0  0  0 0.077 0.143 0.077 0.220 0.112    0.117
#> 612   0  4   0  1  0   0  0  0 0.125 0.125 0.125 0.250 0.110    0.220
#> 613   0  1   0  0  0   1  0  0 0.125 0.125 0.125 0.250 0.110    0.126
#> 614   0  8   1  1  0   0  0  0 0.091 0.130 0.091 0.221 0.108    0.126
#> 615   1 16   0  0  0   0  0  0 0.091 0.130 0.091 0.221 0.106    0.294
#> 616   2  6   0  0  0   0  0  0 0.000 0.154 0.000 0.154 0.106    0.000
#> 617   1  5   0  1  0   0  0  0 0.071 0.133 0.071 0.205 0.105    0.098
#> 618   0  9   0  3  0   1  0  0 0.111 0.111 0.111 0.222 0.098    0.196
#> 619   0  1   0  0  0   1  0  0 0.111 0.111 0.111 0.222 0.098    0.110
#> 620   0  8   0  3  0   1  0  0 0.111 0.111 0.111 0.222 0.098    0.176
#> 621   1  2   0  2  0   0  0  0 0.000 0.143 0.000 0.143 0.098    0.000
#> 622   1  1   0  0  0   0  0  0 0.000 0.143 0.000 0.143 0.098    0.000
#> 623   0  8   0  1  0   0  0  0 0.105 0.105 0.105 0.211 0.093    0.160
#> 624   1 18   0  1  0   0  1  1 0.063 0.091 0.094 0.185 0.086    0.153
#> 625   0  2   0  6  0   0  0  0 0.091 0.091 0.091 0.182 0.080    0.098
#> 626   0  6   0  2  0   0  0  0 0.091 0.091 0.091 0.182 0.080    0.176
#> 627   0  4   0  0  0   0  0  0 0.091 0.091 0.091 0.182 0.080    0.126
#> 628   0  1   1  0  0   0  3  2 0.000 0.111 0.000 0.111 0.080    0.000
#> 629   0  8   0  0  0   0  0  0 0.063 0.063 0.125 0.188 0.078    0.157
#> 630   0  2   0  0  0   1  0  0 0.083 0.083 0.083 0.167 0.073    0.088
#> 631   0  9   0  1  0   1  0  0 0.083 0.083 0.083 0.167 0.073    0.117
#> 632   1  5   0  4  0   1  0  0 0.000 0.100 0.000 0.100 0.069    0.000
#> 633   1  4   0  0  0   0  0  0 0.000 0.100 0.000 0.100 0.069    0.000
#> 634   0  3   0  1  0   0  0  0 0.077 0.077 0.077 0.154 0.068    0.088
#> 635   0  4   0  1  0   0  0  0 0.071 0.071 0.071 0.143 0.063    0.088
#> 636   0  7   0  1  0   0  0  0 0.067 0.067 0.067 0.133 0.059    0.110
#> 637   0  7   0  1  0   1  0  0 0.059 0.059 0.059 0.118 0.052    0.088
#> 638   0  7   0  1  0   0  0  0 0.059 0.059 0.059 0.118 0.052    0.088
#> 639   0 10   0  0  0   0  0  0 0.056 0.056 0.056 0.111 0.049    0.110
#> 640   1  7   0  4  0   0  0  0 0.000 0.071 0.000 0.071 0.049    0.000
#> 641   0  7   0  1  0   0  0  0 0.056 0.056 0.056 0.111 0.049    0.080
#> 642   0 12   0  3  0   0  0  0 0.050 0.050 0.050 0.100 0.044    0.110
#> 643   0  6   0  0  0   0  0  0 0.048 0.048 0.048 0.095 0.042    0.059
#> 644   0  6   0  2  0   0  0  0 0.048 0.048 0.048 0.095 0.042    0.059
#> 645   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 646   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 647   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 648   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 649   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 650   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 651   0  2   0  0  0   2  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 652   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 653   0  1   0  0  0   0  1  1 0.000 0.000 0.000 0.000 0.000      NaN
#> 654   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 655   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 656   0  4   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 657   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 658   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 659   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 660   0  1   0  0  0   0  1  1 0.000 0.000 0.000 0.000 0.000    0.000
#> 661   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 662   0  1   0  2  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 663   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 664   0  2   0  2  1   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 665   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 666   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 667   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 668   0  1   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 669   0  3   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 670   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 671   0  4   0  0  1   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 672   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 673   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 674   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 675   0  0   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 676   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 677   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 678   0  1   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 679   0  2   0  2  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 680   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 681   0  5   0  0  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 682   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 683   0  5   0  3  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 684   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 685   0  3   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 686   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 687   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 688   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 689   0  3   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 690   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 691   0 13   0  0  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 692   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 693   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 694   0  8   0  1  0   2  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 695   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 696   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 697   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 698   0  0   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 699   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 700   0  9   0  4  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 701   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 702   0  2   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 703   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 704   0  1   0  0  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 705   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 706   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 707   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 708   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 709   0  6   0  3  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 710   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 711   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 712   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 713   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 714   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 715   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 716   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 717   0 11   0  2  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 718   0  4   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 719   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 720   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 721   0 14   0  2  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 722   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 723   0  6   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 724   0  2   0  0  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 725   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 726   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 727   0  0   0  0  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 728   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 729   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 730   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 731   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 732   0  2   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 733   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 734   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 735   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 736   0 11   0  1  0   1  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 737   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 738   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 739   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 740   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 741   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 742   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 743   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 744   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 745   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 746   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 747   0  2   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 748   0  2   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 749   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 750   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 751   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 752   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000      NaN
#> 753   0  8   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 754   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 755   0  4   0  2  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 756   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 757   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 758   0  0   0  1  0   0  0  0 0.000 0.000 0.000 0.000 0.000    0.000
#> 759   0  0   0  1  0   0  0  0    NA    NA    NA    NA   NaN      NaN
#> 760   0  0   0  1  0   0  0  0    NA    NA    NA    NA   NaN      NaN
#> 761   0  0   0  1  0   0  0  0    NA    NA    NA    NA   NaN      NaN
#> 762   0  0   0  1  0   0  0  0    NA    NA    NA    NA   NaN      NaN
#> 763   0  0   0  0  0   0  0  0    NA    NA    NA    NA   NaN      NaN
#> 764   0  0   0  2  0   0  0  0    NA    NA    NA    NA   NaN      NaN
# }
```
