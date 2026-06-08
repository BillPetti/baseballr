# **Scrape Pitcher Performance Data Over a Custom Time Frame**

This function allows you to scrape basic pitcher statistics over a
custom time frame. Data is sourced from Baseball-Reference.com.

## Usage

``` r
bref_daily_pitcher(t1, t2)
```

## Arguments

- t1:

  First date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

- t2:

  Last date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

## Value

Returns a tibble of pitcher performance over the requested date range,
one row per player, with the following columns:

|          |           |                                             |
|----------|-----------|---------------------------------------------|
| col_name | types     | description                                 |
| season   | integer   | Season year.                                |
| Name     | character | Player name.                                |
| Age      | numeric   | Player age during the season.               |
| Level    | character | League level (e.g. Maj-AL, Maj-NL).         |
| Team     | character | Team name.                                  |
| G        | numeric   | Games pitched.                              |
| GS       | numeric   | Games started.                              |
| W        | numeric   | Wins.                                       |
| L        | numeric   | Losses.                                     |
| SV       | numeric   | Saves.                                      |
| IP       | numeric   | Innings pitched.                            |
| H        | numeric   | Hits allowed.                               |
| R        | numeric   | Runs allowed.                               |
| ER       | numeric   | Earned runs allowed.                        |
| uBB      | numeric   | Unintentional walks allowed.                |
| BB       | numeric   | Walks allowed.                              |
| SO       | numeric   | Strikeouts.                                 |
| HR       | numeric   | Home runs allowed.                          |
| HBP      | numeric   | Batters hit by pitch.                       |
| ERA      | numeric   | Earned run average (per 9 innings).         |
| AB       | numeric   | At-bats against.                            |
| X1B      | numeric   | Singles allowed.                            |
| X2B      | numeric   | Doubles allowed.                            |
| X3B      | numeric   | Triples allowed.                            |
| IBB      | numeric   | Intentional walks allowed.                  |
| GDP      | numeric   | Double plays induced.                       |
| SF       | numeric   | Sacrifice flies allowed.                    |
| SB       | numeric   | Stolen bases allowed.                       |
| CS       | numeric   | Runners caught stealing.                    |
| PO       | numeric   | Pickoffs.                                   |
| BF       | numeric   | Batters faced.                              |
| Pit      | numeric   | Total pitches thrown.                       |
| Str      | numeric   | Strikes thrown (as a share of pitches).     |
| StL      | numeric   | Looking (called) strikes share.             |
| StS      | numeric   | Swinging strikes share.                     |
| GB.FB    | numeric   | Ground ball to fly ball share.              |
| LD       | numeric   | Line drive share.                           |
| PU       | numeric   | Pop-up (infield fly) share.                 |
| WHIP     | numeric   | Walks plus hits per inning pitched.         |
| BAbip    | numeric   | Batting average on balls in play allowed.   |
| SO9      | numeric   | Strikeouts per 9 innings.                   |
| SO.W     | numeric   | Strikeout-to-walk ratio.                    |
| SO_perc  | numeric   | Strikeout rate (per batter faced).          |
| uBB_perc | numeric   | Unintentional walk rate (per batter faced). |
| SO_uBB   | numeric   | Strikeouts minus unintentional walks.       |

## Examples

``` r
# \donttest{
  try(bref_daily_pitcher("2015-05-10", "2015-06-20"))
#> ✖ 2026-06-08 01:55:12.122564: Invalid arguments or no daily pitcher data available!
#>     season                   Name Age         Level
#> 1     2015          David Aardsma  33        Maj-NL
#> 2     2015          Fernando Abad  29        Maj-AL
#> 3     2015           Austin Adams  28        Maj-AL
#> 4     2015          Nathan Adcock  27        Maj-NL
#> 5     2015         Jeremy Affeldt  36        Maj-NL
#> 6     2015        Al Alburquerque  29        Maj-AL
#> 7     2015             Cody Allen  26        Maj-AL
#> 8     2015  Henderson Alvarez III  25        Maj-NL
#> 9     2015           José Álvarez  26        Maj-AL
#> 10    2015         Alexi Amarista  26        Maj-NL
#> 11    2015         Brett Anderson  27        Maj-NL
#> 12    2015         Chase Anderson  27        Maj-NL
#> 13    2015          Matt Andriese  25        Maj-AL
#> 14    2015           Elvis Araújo  23        Maj-NL
#> 15    2015           Chris Archer  26        Maj-AL
#> 16    2015           Jake Arrieta  29        Maj-NL
#> 17    2015         Scott Atchison  39        Maj-AL
#> 18    2015       Phillippe Aumont  26        Maj-NL
#> 19    2015            Luis Avilán  25        Maj-NL
#> 20    2015            John Axford  32        Maj-NL
#> 21    2015         Burke Badenhop  32        Maj-NL
#> 22    2015             Pedro Báez  27        Maj-NL
#> 23    2015            Matt Barnes  25        Maj-AL
#> 24    2015          Aaron Barrett  27        Maj-NL
#> 25    2015           Anthony Bass  27        Maj-AL
#> 26    2015       Antonio Bastardo  29        Maj-NL
#> 27    2015           Trevor Bauer  24        Maj-AL
#> 28    2015             Chris Beck  24        Maj-AL
#> 29    2015          Cam Bedrosian  23        Maj-AL
#> 30    2015             Joe Beimel  38        Maj-AL
#> 31    2015       Ronald Belisario  32        Maj-AL
#> 32    2015           Matt Belisle  35        Maj-NL
#> 33    2015        Andrew Bellatti  23        Maj-AL
#> 34    2015         Joaquín Benoit  37        Maj-NL
#> 35    2015      Christian Bergman  27        Maj-NL
#> 36    2015        Dellin Betances  27        Maj-AL
#> 37    2015      Rafael Betancourt  40        Maj-NL
#> 38    2015            Chad Bettis  26        Maj-NL
#> 39    2015       Chad Billingsley  30        Maj-NL
#> 40    2015            Joe Blanton  34        Maj-AL
#> 41    2015         Michael Blazek  26        Maj-NL
#> 42    2015         Mike Bolsinger  27        Maj-NL
#> 43    2015         Brad Boxberger  27        Maj-AL
#> 44    2015           Blaine Boyer  33        Maj-AL
#> 45    2015             Brad Brach  29        Maj-AL
#> 46    2015         Archie Bradley  22        Maj-NL
#> 47    2015          Craig Breslow  34        Maj-AL
#> 48    2015           Zack Britton  27        Maj-AL
#> 49    2015          Mike Broadway  28        Maj-NL
#> 50    2015           Aaron Brooks  25        Maj-AL
#> 51    2015           Brooks Brown  30        Maj-NL
#> 52    2015       Jonathan Broxton  31        Maj-NL
#> 53    2015          Jake Buchanan  25        Maj-AL
#> 54    2015          Clay Buchholz  30        Maj-AL
#> 55    2015           Mark Buehrle  36        Maj-AL
#> 56    2015      Madison Bumgarner  25        Maj-NL
#> 57    2015         Enrique Burgos  24        Maj-NL
#> 58    2015           A.J. Burnett  38        Maj-NL
#> 59    2015           Eddie Butler  24        Maj-NL
#> 60    2015           César Cabral  26        Maj-AL
#> 61    2015          Trevor Cahill  27        Maj-NL
#> 62    2015    Arquimedes Caminero  28        Maj-NL
#> 63    2015           Carter Capps  24        Maj-NL
#> 64    2015          Chris Capuano  36        Maj-AL
#> 65    2015          Buddy Carlyle  37        Maj-NL
#> 66    2015        David Carpenter  29 Maj-AL,Maj-NL
#> 67    2015        Carlos Carrasco  28        Maj-AL
#> 68    2015          Scott Carroll  30        Maj-AL
#> 69    2015         Andrew Cashner  28        Maj-NL
#> 70    2015       Santiago Casilla  34        Maj-NL
#> 71    2015           Ángel Castro  32        Maj-AL
#> 72    2015            Brett Cecil  28        Maj-AL
#> 73    2015          Xavier Cedeño  28        Maj-AL
#> 74    2015          Andrew Chafin  25        Maj-NL
#> 75    2015       Joba Chamberlain  29        Maj-AL
#> 76    2015        Aroldis Chapman  27        Maj-NL
#> 77    2015          Kevin Chapman  27        Maj-AL
#> 78    2015           Jesse Chavez  31        Maj-AL
#> 79    2015             Bruce Chen  38        Maj-AL
#> 80    2015           Wei-Yin Chen  29        Maj-AL
#> 81    2015           Randy Choate  39        Maj-NL
#> 82    2015          Tony Cingrani  25        Maj-NL
#> 83    2015           Steve Cishek  29        Maj-NL
#> 84    2015           Alex Claudio  23        Maj-AL
#> 85    2015         Tyler Clippard  30        Maj-AL
#> 86    2015              Phil Coke  32 Maj-AL,Maj-NL
#> 87    2015              A.J. Cole  23        Maj-NL
#> 88    2015            Gerrit Cole  24        Maj-NL
#> 89    2015        Josh Collmenter  29        Maj-NL
#> 90    2015            Alex Colomé  26        Maj-AL
#> 91    2015          Bartolo Colón  42        Maj-NL
#> 92    2015            Adam Conley  25        Maj-NL
#> 93    2015       Carlos Contreras  24        Maj-NL
#> 94    2015         Scott Copeland  27        Maj-AL
#> 95    2015          Kevin Correia  34        Maj-NL
#> 96    2015          Jarred Cosart  25        Maj-NL
#> 97    2015             Neal Cotts  35        Maj-NL
#> 98    2015         Danny Coulombe  25        Maj-NL
#> 99    2015            Tyler Cravy  25        Maj-NL
#> 100   2015          Kyle Crockett  23        Maj-AL
#> 101   2015           Johnny Cueto  29        Maj-NL
#> 102   2015        Brandon Cunniff  26        Maj-NL
#> 103   2015             John Danks  30        Maj-AL
#> 104   2015             Wade Davis  29        Maj-AL
#> 105   2015       Justin De Fratus  27        Maj-NL
#> 106   2015       Jorge De La Rosa  34        Maj-NL
#> 107   2015       Rubby De La Rosa  26        Maj-NL
#> 108   2015             Sam Deduno  31        Maj-AL
#> 109   2015           Jacob deGrom  27        Maj-NL
#> 110   2015          Steve Delabar  31        Maj-AL
#> 111   2015        Randall Delgado  25        Maj-NL
#> 112   2015     Anthony DeSclafani  25        Maj-NL
#> 113   2015    Odrisamer Despaigne  28        Maj-NL
#> 114   2015          Ross Detwiler  29        Maj-AL
#> 115   2015             Jumbo Díaz  31        Maj-NL
#> 116   2015            R.A. Dickey  40        Maj-AL
#> 117   2015           Jake Diekman  28        Maj-NL
#> 118   2015         Sean Doolittle  28        Maj-AL
#> 119   2015           Oliver Drake  28        Maj-AL
#> 120   2015         Brian Duensing  32        Maj-AL
#> 121   2015            Danny Duffy  26        Maj-AL
#> 122   2015              Zach Duke  32        Maj-AL
#> 123   2015              Mike Dunn  30        Maj-NL
#> 124   2015              Sam Dyson  27        Maj-NL
#> 125   2015            Jon Edwards  27        Maj-AL
#> 126   2015           Roenis Elías  26        Maj-AL
#> 127   2015            Jake Elmore  28        Maj-AL
#> 128   2015         Nathan Eovaldi  25        Maj-AL
#> 129   2015          Marco Estrada  31        Maj-AL
#> 130   2015           Dana Eveland  31        Maj-NL
#> 131   2015         Jeurys Familia  25        Maj-NL
#> 132   2015            Buck Farmer  24        Maj-AL
#> 133   2015         Danny Farquhar  28        Maj-AL
#> 134   2015          Scott Feldman  32        Maj-AL
#> 135   2015          Michael Feliz  22        Maj-AL
#> 136   2015          Neftalí Feliz  27        Maj-AL
#> 137   2015            Josh Fields  29        Maj-AL
#> 138   2015             Casey Fien  31        Maj-AL
#> 139   2015             Mike Fiers  30        Maj-NL
#> 140   2015       Brandon Finnegan  22        Maj-AL
#> 141   2015            Doug Fister  31        Maj-NL
#> 142   2015           Yohan Flande  29        Maj-NL
#> 143   2015          Kendry Flores  23        Maj-NL
#> 144   2015       Mike Foltynewicz  23        Maj-NL
#> 145   2015           Jeff Francis  34        Maj-AL
#> 146   2015         Jeff Francoeur  31        Maj-NL
#> 147   2015          Nick Franklin  24        Maj-AL
#> 148   2015           Jason Frasor  37        Maj-AL
#> 149   2015            Sam Freeman  28        Maj-AL
#> 150   2015           Carlos Frías  25        Maj-NL
#> 151   2015    Christian Friedrich  27        Maj-NL
#> 152   2015         Ernesto Frieri  29        Maj-AL
#> 153   2015         Kyuji Fujikawa  34        Maj-AL
#> 154   2015        Charlie Furbush  29        Maj-AL
#> 155   2015        Yovani Gallardo  29        Maj-AL
#> 156   2015           Frank Garcés  25        Maj-NL
#> 157   2015           Jaime García  28        Maj-NL
#> 158   2015           Jason García  22        Maj-AL
#> 159   2015            Luis Garcia  28        Maj-NL
#> 160   2015            Yimi García  24        Maj-NL
#> 161   2015             Matt Garza  31        Maj-NL
#> 162   2015          Kevin Gausman  24        Maj-AL
#> 163   2015             Dillon Gee  29        Maj-NL
#> 164   2015            Steve Geltz  27        Maj-AL
#> 165   2015            Kyle Gibson  27        Maj-AL
#> 166   2015              Ken Giles  24        Maj-NL
#> 167   2015         Sean Gilmartin  25        Maj-NL
#> 168   2015           Erik Goeddel  26        Maj-NL
#> 169   2015          David Goforth  26        Maj-NL
#> 170   2015          Brandon Gomes  30        Maj-AL
#> 171   2015          Jeanmar Gómez  27        Maj-NL
#> 172   2015       Chi Chi González  23        Maj-AL
#> 173   2015           Gio Gonzalez  29        Maj-NL
#> 174   2015        Miguel González  31        Maj-AL
#> 175   2015      Severino González  22        Maj-NL
#> 176   2015         Tom Gorzelanny  32        Maj-AL
#> 177   2015            Trevor Gott  22        Maj-AL
#> 178   2015             Matt Grace  26        Maj-NL
#> 179   2015            J.R. Graham  25        Maj-AL
#> 180   2015       Kendall Graveman  24        Maj-AL
#> 181   2015             Sonny Gray  25        Maj-AL
#> 182   2015           Shane Greene  26        Maj-AL
#> 183   2015         Luke Gregerson  31        Maj-AL
#> 184   2015           Zack Greinke  31        Maj-NL
#> 185   2015           Jason Grilli  38        Maj-NL
#> 186   2015           Justin Grimm  26        Maj-NL
#> 187   2015         Mayckol Guaipe  24        Maj-AL
#> 188   2015          Junior Guerra  30        Maj-AL
#> 189   2015        Preston Guilmet  27        Maj-AL
#> 190   2015         Jeremy Guthrie  36        Maj-AL
#> 191   2015          Nick Hagadone  29        Maj-AL
#> 192   2015             Jesse Hahn  25        Maj-AL
#> 193   2015             David Hale  27        Maj-NL
#> 194   2015            Cole Hamels  31        Maj-NL
#> 195   2015           Jason Hammel  32        Maj-NL
#> 196   2015              Brad Hand  25        Maj-NL
#> 197   2015           Donovan Hand  29        Maj-NL
#> 198   2015              J.A. Happ  32        Maj-AL
#> 199   2015           Aaron Harang  37        Maj-NL
#> 200   2015           Blaine Hardy  28        Maj-AL
#> 201   2015              Dan Haren  34        Maj-NL
#> 202   2015           Mitch Harris  29        Maj-NL
#> 203   2015            Will Harris  30        Maj-AL
#> 204   2015            Matt Harvey  26        Maj-NL
#> 205   2015          Chris Hatcher  30        Maj-NL
#> 206   2015         LaTroy Hawkins  42        Maj-NL
#> 207   2015      Jeremy Hellickson  28        Maj-NL
#> 208   2015          Heath Hembree  26        Maj-AL
#> 209   2015         Kyle Hendricks  25        Maj-NL
#> 210   2015          Liam Hendriks  26        Maj-AL
#> 211   2015        David Hernandez  30        Maj-NL
#> 212   2015        Félix Hernández  29        Maj-AL
#> 213   2015      Roberto Hernández  34        Maj-AL
#> 214   2015         Kelvin Herrera  25        Maj-AL
#> 215   2015           Chris Heston  27        Maj-NL
#> 216   2015            Taylor Hill  26        Maj-NL
#> 217   2015          Luke Hochevar  31        Maj-AL
#> 218   2015           Greg Holland  29        Maj-AL
#> 219   2015            J.J. Hoover  27        Maj-NL
#> 220   2015            J.P. Howell  32        Maj-NL
#> 221   2015          Daniel Hudson  28        Maj-NL
#> 222   2015             Tim Hudson  39        Maj-NL
#> 223   2015             David Huff  30        Maj-NL
#> 224   2015           Jared Hughes  29        Maj-NL
#> 225   2015            Phil Hughes  29        Maj-AL
#> 226   2015           Tommy Hunter  28        Maj-AL
#> 227   2015         Drew Hutchison  24        Maj-AL
#> 228   2015           Edgar Ibarra  26        Maj-AL
#> 229   2015        Raisel Iglesias  25        Maj-NL
#> 230   2015          Edwin Jackson  31        Maj-NL
#> 231   2015          Kenley Jansen  27        Maj-NL
#> 232   2015          Casey Janssen  33        Maj-NL
#> 233   2015        Jeremy Jeffress  27        Maj-NL
#> 234   2015           Dan Jennings  28        Maj-AL
#> 235   2015           Kevin Jepsen  30        Maj-AL
#> 236   2015         Ubaldo Jiménez  31        Maj-AL
#> 237   2015            Jim Johnson  32        Maj-NL
#> 238   2015          Garrett Jones  34        Maj-AL
#> 239   2015          Taylor Jordan  26        Maj-NL
#> 240   2015        Taylor Jungmann  25        Maj-NL
#> 241   2015           Tommy Kahnle  25        Maj-NL
#> 242   2015             Nate Karns  27        Maj-AL
#> 243   2015           Scott Kazmir  31        Maj-AL
#> 244   2015             Keone Kela  22        Maj-AL
#> 245   2015           Shawn Kelley  31        Maj-NL
#> 246   2015              Joe Kelly  27        Maj-AL
#> 247   2015          Kyle Kendrick  30        Maj-NL
#> 248   2015            Ian Kennedy  30        Maj-NL
#> 249   2015        Clayton Kershaw  27        Maj-NL
#> 250   2015         Dallas Keuchel  27        Maj-AL
#> 251   2015          Craig Kimbrel  27        Maj-NL
#> 252   2015       Brandon Kintzler  30        Maj-NL
#> 253   2015             Phil Klein  26        Maj-AL
#> 254   2015           Corey Kluber  29        Maj-AL
#> 255   2015           Corey Knebel  23        Maj-NL
#> 256   2015            Tom Koehler  29        Maj-NL
#> 257   2015          George Kontos  30        Maj-NL
#> 258   2015               Ian Krol  24        Maj-AL
#> 259   2015            John Lackey  36        Maj-NL
#> 260   2015      Bobby LaFromboise  29        Maj-NL
#> 261   2015              Mat Latos  27        Maj-NL
#> 262   2015            Tommy Layne  30        Maj-AL
#> 263   2015             Mike Leake  27        Maj-NL
#> 264   2015       Jack Leathersich  24        Maj-NL
#> 265   2015               C.C. Lee  28        Maj-AL
#> 266   2015            Arnold Leon  26        Maj-AL
#> 267   2015          Dominic Leone  23 Maj-AL,Maj-NL
#> 268   2015             Jon Lester  31        Maj-NL
#> 269   2015            Colby Lewis  35        Maj-AL
#> 270   2015        Adam Liberatore  28        Maj-NL
#> 271   2015           Tim Lincecum  31        Maj-NL
#> 272   2015         Jacob Lindgren  22        Maj-AL
#> 273   2015      Francisco Liriano  31        Maj-NL
#> 274   2015           Radhames Liz  31        Maj-NL
#> 275   2015          Kyle Lobstein  25        Maj-AL
#> 276   2015             Jeff Locke  27        Maj-NL
#> 277   2015            Boone Logan  30        Maj-NL
#> 278   2015             Kyle Lohse  36        Maj-NL
#> 279   2015           Javier López  37        Maj-NL
#> 280   2015       Michael Lorenzen  23        Maj-NL
#> 281   2015             Aaron Loup  27        Maj-AL
#> 282   2015              Mark Lowe  32        Maj-AL
#> 283   2015           Jordan Lyles  24        Maj-NL
#> 284   2015             Lance Lynn  28        Maj-NL
#> 285   2015            Tyler Lyons  27        Maj-NL
#> 286   2015             Jean Machi  33        Maj-NL
#> 287   2015            Ryan Madson  34        Maj-AL
#> 288   2015            Seth Maness  26        Maj-NL
#> 289   2015           Jeff Manship  30        Maj-AL
#> 290   2015           Shaun Marcum  33        Maj-AL
#> 291   2015      Sugar Ray Marimón  26        Maj-NL
#> 292   2015          Jason Marquis  36        Maj-NL
#> 293   2015          Evan Marshall  25        Maj-NL
#> 294   2015           Chris Martin  29        Maj-AL
#> 295   2015            Cody Martin  25        Maj-NL
#> 296   2015        Carlos Martínez  23        Maj-NL
#> 297   2015          Nick Martinez  24        Maj-AL
#> 298   2015            Nick Masset  33        Maj-NL
#> 299   2015       Justin Masterson  30        Maj-AL
#> 300   2015          Ryan Mattheus  31        Maj-NL
#> 301   2015           Brian Matusz  28        Maj-AL
#> 302   2015         Brandon Maurer  24        Maj-NL
#> 303   2015             Trevor May  25        Maj-AL
#> 304   2015            Vin Mazzaro  28        Maj-NL
#> 305   2015           Cory Mazzoni  25        Maj-NL
#> 306   2015        Zach McAllister  27        Maj-AL
#> 307   2015    Lance McCullers Jr.  21        Maj-AL
#> 308   2015         T.J. McFarland  26        Maj-AL
#> 309   2015             Jake McGee  28        Maj-AL
#> 310   2015         Dustin McGowan  33        Maj-NL
#> 311   2015          Collin McHugh  28        Maj-AL
#> 312   2015         Yoervis Medina  26        Maj-NL
#> 313   2015          Mark Melancon  30        Maj-NL
#> 314   2015             Wade Miley  28        Maj-AL
#> 315   2015          Andrew Miller  30        Maj-AL
#> 316   2015          Justin Miller  28        Maj-NL
#> 317   2015          Shelby Miller  24        Maj-NL
#> 318   2015           Tommy Milone  28        Maj-AL
#> 319   2015         Bryan Mitchell  24        Maj-AL
#> 320   2015        Mike Montgomery  25        Maj-AL
#> 321   2015       Franklin Morales  29        Maj-AL
#> 322   2015             Mike Morin  24        Maj-AL
#> 323   2015           Akeel Morris  22        Maj-NL
#> 324   2015           Bryan Morris  28        Maj-NL
#> 325   2015         Charlie Morton  31        Maj-NL
#> 326   2015             Jon Moscot  23        Maj-NL
#> 327   2015            Jason Motte  33        Maj-NL
#> 328   2015          Edward Mujica  31        Maj-AL
#> 329   2015           David Murphy  33        Maj-AL
#> 330   2015           Jimmy Nelson  26        Maj-NL
#> 331   2015          Ángel Nesbitt  24        Maj-AL
#> 332   2015             Pat Neshek  34        Maj-AL
#> 333   2015           Juan Nicasio  28        Maj-NL
#> 334   2015        Justin Nicolino  23        Maj-NL
#> 335   2015              Jon Niese  28        Maj-NL
#> 336   2015           Héctor Noesí  28        Maj-AL
#> 337   2015          Ricky Nolasco  32        Maj-AL
#> 338   2015             Bud Norris  30        Maj-AL
#> 339   2015         Vidal Nuño III  27 Maj-AL,Maj-NL
#> 340   2015           Darren O'Day  32        Maj-AL
#> 341   2015        Eric O'Flaherty  30        Maj-AL
#> 342   2015        Sean O'Sullivan  27        Maj-NL
#> 343   2015            Scott Oberg  25        Maj-NL
#> 344   2015      Brett Oberholtzer  25        Maj-AL
#> 345   2015          Jake Odorizzi  25        Maj-AL
#> 346   2015           Alexi Ogando  31        Maj-AL
#> 347   2015         Ross Ohlendorf  32        Maj-AL
#> 348   2015          Roberto Osuna  20        Maj-AL
#> 349   2015              Dan Otero  30        Maj-AL
#> 350   2015      Jonathan Papelbon  34        Maj-NL
#> 351   2015          Bobby Parnell  30        Maj-NL
#> 352   2015            Manny Parra  32        Maj-NL
#> 353   2015           James Paxton  26        Maj-AL
#> 354   2015           Mike Pelfrey  31        Maj-AL
#> 355   2015           Wily Peralta  26        Maj-NL
#> 356   2015           Óliver Pérez  33        Maj-NL
#> 357   2015         Williams Pérez  24        Maj-NL
#> 358   2015           Glen Perkins  32        Maj-AL
#> 359   2015         Vinnie Pestano  30        Maj-AL
#> 360   2015         Yusmeiro Petit  30        Maj-NL
#> 361   2015          Jake Petricka  27        Maj-AL
#> 362   2015           David Phelps  28        Maj-NL
#> 363   2015        Stolmy Pimentel  25        Maj-AL
#> 364   2015         Branden Pinder  26        Maj-AL
#> 365   2015         Michael Pineda  26        Maj-AL
#> 366   2015             Yohan Pino  31        Maj-AL
#> 367   2015          Drew Pomeranz  26        Maj-AL
#> 368   2015          Rick Porcello  26        Maj-AL
#> 369   2015           Ryan Pressly  26        Maj-AL
#> 370   2015            David Price  29        Maj-AL
#> 371   2015            Zach Putnam  27        Maj-AL
#> 372   2015      Kevin Quackenbush  26        Maj-NL
#> 373   2015            Chad Qualls  36        Maj-AL
#> 374   2015          Jose Quintana  26        Maj-AL
#> 375   2015            Ryan Raburn  34        Maj-AL
#> 376   2015         Erasmo Ramírez  25        Maj-AL
#> 377   2015             JC Ramirez  26        Maj-NL
#> 378   2015           José Ramírez  25        Maj-AL
#> 379   2015               AJ Ramos  28        Maj-NL
#> 380   2015            Cesár Ramos  31        Maj-AL
#> 381   2015        Anthony Ranaudo  25        Maj-AL
#> 382   2015             Josh Ravin  27        Maj-NL
#> 383   2015             Robbie Ray  23        Maj-NL
#> 384   2015           Todd Redmond  30        Maj-AL
#> 385   2015           Addison Reed  26        Maj-NL
#> 386   2015       Garrett Richards  27        Maj-AL
#> 387   2015      C.J. Riefenhauser  25        Maj-AL
#> 388   2015           André Rienzo  26        Maj-NL
#> 389   2015           Tanner Roark  28        Maj-NL
#> 390   2015          Kenny Roberts  27        Maj-NL
#> 391   2015        David Robertson  30        Maj-AL
#> 392   2015         Clint Robinson  30        Maj-NL
#> 393   2015          Hansel Robles  24        Maj-NL
#> 394   2015        Fernando Rodney  38        Maj-AL
#> 395   2015           Carlos Rodón  22        Maj-AL
#> 396   2015      Eduardo Rodríguez  22        Maj-AL
#> 397   2015 Fernando Rodriguez Jr.  31        Maj-AL
#> 398   2015    Francisco Rodríguez  33        Maj-NL
#> 399   2015         Paco Rodríguez  24        Maj-NL
#> 400   2015        Wandy Rodríguez  36        Maj-AL
#> 401   2015               Chaz Roe  28        Maj-AL
#> 402   2015           Esmil Rogers  29        Maj-AL
#> 403   2015            Enny Romero  24        Maj-AL
#> 404   2015            Sergio Romo  32        Maj-NL
#> 405   2015          Héctor Rondón  27        Maj-NL
#> 406   2015       Trevor Rosenthal  25        Maj-NL
#> 407   2015             Seth Rosin  26        Maj-NL
#> 408   2015               Joe Ross  22        Maj-NL
#> 409   2015            Robbie Ross  26        Maj-AL
#> 410   2015             Tyson Ross  28        Maj-NL
#> 411   2015            Zac Rosscup  27        Maj-NL
#> 412   2015            Chris Rusin  28        Maj-NL
#> 413   2015          James Russell  29        Maj-NL
#> 414   2015              Kyle Ryan  23        Maj-AL
#> 415   2015       Marc Rzepczynski  29        Maj-AL
#> 416   2015            CC Sabathia  34        Maj-AL
#> 417   2015         Fernando Salas  30        Maj-AL
#> 418   2015          Danny Salazar  25        Maj-AL
#> 419   2015             Chris Sale  26        Maj-AL
#> 420   2015        Jeff Samardzija  30        Maj-AL
#> 421   2015          Aaron Sanchez  22        Maj-AL
#> 422   2015         Aníbal Sánchez  31        Maj-AL
#> 423   2015        Héctor Santiago  27        Maj-AL
#> 424   2015          Sergio Santos  31 Maj-AL,Maj-NL
#> 425   2015            Rob Scahill  28        Maj-NL
#> 426   2015       Tanner Scheppers  28        Maj-AL
#> 427   2015           Max Scherzer  30        Maj-NL
#> 428   2015        Brian Schlitter  29        Maj-NL
#> 429   2015             Bo Schultz  29        Maj-AL
#> 430   2015          Evan Scribner  29        Maj-AL
#> 431   2015             Bryan Shaw  27        Maj-AL
#> 432   2015          James Shields  33        Maj-NL
#> 433   2015         Matt Shoemaker  28        Maj-AL
#> 434   2015          Chasen Shreve  24        Maj-AL
#> 435   2015         Kevin Siegrist  25        Maj-NL
#> 436   2015          Alfredo Simón  34        Maj-AL
#> 437   2015              Tony Sipp  31        Maj-AL
#> 438   2015           Carson Smith  25        Maj-AL
#> 439   2015              Joe Smith  31        Maj-AL
#> 440   2015             Will Smith  25        Maj-NL
#> 441   2015      Miguel Socolovich  28        Maj-NL
#> 442   2015            Sammy Solís  26        Maj-NL
#> 443   2015           Joakim Soria  31        Maj-AL
#> 444   2015           Tim Stauffer  33        Maj-AL
#> 445   2015            Drew Storen  27        Maj-NL
#> 446   2015      Stephen Strasburg  26        Maj-NL
#> 447   2015          Huston Street  31        Maj-AL
#> 448   2015      Hunter Strickland  26        Maj-NL
#> 449   2015            Pedro Strop  30        Maj-NL
#> 450   2015            Eric Stults  35        Maj-NL
#> 451   2015            Jesús Sucre  27        Maj-AL
#> 452   2015       Noah Syndergaard  22        Maj-NL
#> 453   2015        Masahiro Tanaka  26        Maj-AL
#> 454   2015         Junichi Tazawa  29        Maj-AL
#> 455   2015          Julio Teheran  24        Maj-NL
#> 456   2015            Ryan Tepera  27        Maj-AL
#> 457   2015           Joe Thatcher  33        Maj-AL
#> 458   2015            Dale Thayer  34        Maj-NL
#> 459   2015             Ian Thomas  28        Maj-NL
#> 460   2015         Aaron Thompson  28        Maj-AL
#> 461   2015          Matt Thornton  38        Maj-NL
#> 462   2015          Chris Tillman  27        Maj-AL
#> 463   2015         Shawn Tolleson  27        Maj-AL
#> 464   2015         Michael Tonkin  25        Maj-AL
#> 465   2015            Alex Torres  27        Maj-NL
#> 466   2015          Carlos Torres  32        Maj-NL
#> 467   2015          Blake Treinen  27        Maj-NL
#> 468   2015         Sam Tuivailala  22        Maj-NL
#> 469   2015            Koji Uehara  40        Maj-AL
#> 470   2015             José Ureña  23        Maj-NL
#> 471   2015           Jason Vargas  32        Maj-AL
#> 472   2015         Felipe Vazquez  23        Maj-NL
#> 473   2015            Donnie Veal  30        Maj-NL
#> 474   2015        Vince Velasquez  23        Maj-AL
#> 475   2015           Pat Venditte  30        Maj-AL
#> 476   2015        Yordano Ventura  24        Maj-AL
#> 477   2015       Justin Verlander  32        Maj-AL
#> 478   2015          Logan Verrett  25        Maj-NL
#> 479   2015      Carlos Villanueva  31        Maj-NL
#> 480   2015       Pedro Villarreal  27        Maj-NL
#> 481   2015           Nick Vincent  28        Maj-NL
#> 482   2015         Ryan Vogelsong  37        Maj-NL
#> 483   2015        Edinson Vólquez  31        Maj-AL
#> 484   2015          Michael Wacha  23        Maj-NL
#> 485   2015          Tsuyoshi Wada  34        Maj-NL
#> 486   2015           Tyler Wagner  24        Maj-NL
#> 487   2015         Taijuan Walker  22        Maj-AL
#> 488   2015            Adam Warren  27        Maj-AL
#> 489   2015            Tony Watson  30        Maj-NL
#> 490   2015           Jered Weaver  32        Maj-AL
#> 491   2015            Daniel Webb  25        Maj-AL
#> 492   2015              Ryan Webb  29        Maj-AL
#> 493   2015          Allen Webster  25        Maj-NL
#> 494   2015              Matt West  26        Maj-NL
#> 495   2015          Chase Whitley  26        Maj-AL
#> 496   2015         Tom Wilhelmsen  31        Maj-AL
#> 497   2015        Jerome Williams  33        Maj-NL
#> 498   2015            Alex Wilson  28        Maj-AL
#> 499   2015            C.J. Wilson  34        Maj-AL
#> 500   2015            Josh Wilson  34        Maj-AL
#> 501   2015          Justin Wilson  27        Maj-AL
#> 502   2015           Tyler Wilson  25        Maj-AL
#> 503   2015            Matt Wisler  22        Maj-NL
#> 504   2015              Alex Wood  24        Maj-NL
#> 505   2015            Travis Wood  28        Maj-NL
#> 506   2015           Vance Worley  27        Maj-NL
#> 507   2015        Mike Wright Jr.  25        Maj-AL
#> 508   2015          Steven Wright  30        Maj-AL
#> 509   2015            Chris Young  36        Maj-AL
#> 510   2015           Brad Ziegler  35        Maj-NL
#> 511   2015      Jordan Zimmermann  29        Maj-NL
#>                     Team  G GS  W  L SV   IP  H  R ER uBB BB SO HR HBP
#> 1                Atlanta  5  0 NA NA NA  3.2  2  1  1   2  3  2  0   0
#> 2                Oakland 13  0 NA  1 NA  8.0  6  4  4   1  1  7  3   0
#> 3              Cleveland  5  0 NA NA  1  6.2  6  3  2   2  2  6  0   0
#> 4             Cincinnati  4  0 NA NA NA  5.1  6  4  4   4  4  4  1   0
#> 5          San Francisco 18  0 NA  1 NA 14.2 20 10  9   5  6 11  4   0
#> 6                Detroit 16  0 NA NA NA 19.1 14  2  2   6  7 25  1   0
#> 7              Cleveland 18  0 NA NA 10 19.2 11  3  3   7  7 29  0   0
#> 8                  Miami  2  2 NA  2 NA 10.1 15 12 10   5  6  4  0   0
#> 9            Los Angeles 13  0  1 NA NA 13.1 11  6  6   4  4 14  0   0
#> 10             San Diego  1  0 NA NA NA  0.1  0  0  0   0  0  0  0   0
#> 11           Los Angeles  7  7 NA  3 NA 45.1 44 17 17  15 16 35  4   0
#> 12               Arizona  7  7  3 NA NA 43.0 43 13 13   8  8 22  1   5
#> 13             Tampa Bay  3  1  2 NA  1 13.2 12  3  1   1  2 11  1   0
#> 14          Philadelphia 15  0  1  1 NA 12.2 17  8  5   5  6 12  0   0
#> 15             Tampa Bay  8  8  5 NA NA 53.1 43 12 10   9  9 66  2   0
#> 16               Chicago  7  7  3  2 NA 45.0 40 20 17  12 12 51  6   2
#> 17             Cleveland  9  0  1 NA NA  8.0 12  9  9   2  2  4  5   1
#> 18          Philadelphia  1  1 NA  1 NA  4.0  5  6  6   7  7  3  2   0
#> 19               Atlanta 21  0  2  2 NA 14.2 14  6  6   3  4 13  2   0
#> 20              Colorado 16  0  1 NA  9 15.2  8  3  2   5  5 13  1   0
#> 21            Cincinnati 13  0  1  1 NA 16.2 15  5  5   4  5  8  0   0
#> 22           Los Angeles  2  0  1 NA NA  2.1  2  0  0   0  0  4  0   0
#> 23                Boston 13  0  2  2 NA 13.2 19  9  7   4  4 11  3   0
#> 24            Washington 14  0  1  2 NA  8.2 10  7  7   2  2  8  1   1
#> 25                 Texas  8  0 NA NA NA 13.2 16 10 10   5  5  6  1   0
#> 26            Pittsburgh 11  0  2  1 NA  9.2  8  3  3   0  1  6  2   1
#> 27             Cleveland  7  7  4  2 NA 47.1 31 13 13  21 21 48  4   0
#> 28               Chicago  1  1 NA  1 NA  6.0 10  5  4   4  4  3  0   0
#> 29           Los Angeles 12  0 NA NA NA 11.0 16  7  6   5  7  9  0   1
#> 30               Seattle 14  0 NA NA  1 14.2 13  8  7   5  7 10  2   0
#> 31             Tampa Bay  2  0 NA NA NA  4.0  1  0  0   1  1  2  0   0
#> 32             St. Louis 14  0 NA  1 NA 15.0 18  4  4   5  7 14  0   0
#> 33             Tampa Bay  3  0  1 NA NA  6.2  5  2  2   6  6  8  1   0
#> 34             San Diego 14  0  1  2 NA 14.0  5  3  3   4  4 11  2   0
#> 35              Colorado  9  0  1 NA NA 17.0 20 11 11   4  4 14  2   0
#> 36              New York 15  0 NA NA  3 17.1  4  2  1   7  8 29  0   0
#> 37              Colorado 11  0  2  2 NA  8.1 13 11 11   2  2  6  2   0
#> 38              Colorado  8  8  3  2 NA 49.2 46 20 19  15 15 43  3   1
#> 39          Philadelphia  2  2 NA  1 NA 11.0 15  7  7   2  2  5  1   0
#> 40           Kansas City  8  1  1 NA NA 20.0 19  5  4   2  3 17  1   0
#> 41             Milwaukee 17  0  2  1 NA 21.2 13  6  5   4  5 18  0   1
#> 42           Los Angeles  8  8  4  2 NA 47.2 39 16 16  18 18 44  3   1
#> 43             Tampa Bay 16  0  2  2 10 14.2 16  6  6   6  7 18  3   0
#> 44             Minnesota 18  0  1  2  1 18.1 15  6  4   2  5  7  2   0
#> 45             Baltimore 14  0  2  2 NA 18.1  9  5  5   5  5 23  1   0
#> 46               Arizona  4  4 NA  3 NA 15.2 27 19 19  10 11  9  3   1
#> 47                Boston  8  0 NA NA NA  9.1 13  6  6   3  4  6  1   1
#> 48             Baltimore 16  0 NA NA 13 16.2 17  4  4   2  2 19  1   0
#> 49         San Francisco  3  0 NA NA NA  2.2  3  1  1   2  2  3  0   0
#> 50           Kansas City  1  0 NA NA NA  1.0  0  0  0   0  0  1  0   0
#> 51              Colorado 12  0 NA  1 NA  9.1  6  3  3   6  6  7  1   1
#> 52             Milwaukee 16  0  1  2 NA 15.0 18 13 13   5  6 14  2   0
#> 53               Houston  5  0 NA NA NA  9.0  5  2  2   4  4  5  1   1
#> 54                Boston  8  8  3  2 NA 54.2 49 19 16  12 12 44  2   2
#> 55               Toronto  8  8  3  2 NA 59.1 45 19 18   7 10 31  5   1
#> 56         San Francisco  7  7  4  2 NA 49.2 43 16 16   8  9 46  6   1
#> 57               Arizona  8  0 NA  1  2  7.2  8  4  3   2  2 16  1   0
#> 58            Pittsburgh  8  8  5  2 NA 54.1 54 17 14  13 13 50  1   1
#> 59              Colorado  5  5  1  3 NA 23.0 36 19 16  10 11 10  5   1
#> 60             Baltimore  2  0 NA NA NA  1.0  0  0  0   1  1  1  0   0
#> 61               Atlanta  8  0 NA NA NA  9.2 12  6  6   3  3  7  0   0
#> 62            Pittsburgh 15  0 NA NA NA 16.1  5  3  3   7  7 17  0   0
#> 63                 Miami 13  0  1 NA NA 15.1  8  2  2   4  4 29  1   2
#> 64              New York  8  3 NA  4 NA 21.0 26 14 12   6  6 21  2   1
#> 65              New York  1  0 NA NA NA  0.1  0  0  0   1  1  0  0   0
#> 66   New York,Washington 14  0 NA NA NA 10.2 11  4  3   5  5  6  1   1
#> 67             Cleveland  8  8  4  4 NA 52.0 55 24 24  10 10 55  5   1
#> 68               Chicago  4  0  1 NA NA  6.2 10  3  3   4  5  2  1   0
#> 69             San Diego  8  8  1  3 NA 48.0 56 32 27  15 16 39  7   0
#> 70         San Francisco 16  0  1  1 11 13.1 11  5  4   6  6 17  2   0
#> 71               Oakland  3  0 NA  1 NA  2.1  5  1  1   2  2  2  1   0
#> 72               Toronto 13  0 NA  1  3 13.0 12  7  7   4  4 16  2   1
#> 73             Tampa Bay 19  0  1 NA NA 11.2 16  6  5   5  5  8  1   0
#> 74               Arizona 14  0  3 NA NA 16.2 16  5  5   3  3  9  0   0
#> 75               Detroit 15  0 NA NA NA 10.0 11  3  2   2  2  6  0   0
#> 76            Cincinnati 18  0  2  3  8 17.2 15  7  7  12 12 31  1   1
#> 77               Houston  1  0 NA NA NA  1.1  2  1  1   2  2  3  0   0
#> 78               Oakland  8  8  2  4 NA 53.2 50 18 17  11 11 45  3   0
#> 79             Cleveland  1  1 NA NA NA  2.1  7  3  3   0  0  1  2   0
#> 80             Baltimore  7  7  2  3 NA 45.1 45 16 16   7  7 44  6   0
#> 81             St. Louis 18  0 NA NA NA  8.0  5  2  2   1  1  3  1   2
#> 82            Cincinnati 16  0 NA  3 NA 13.2 13  7  7   9 10 16  1   2
#> 83                 Miami 11  0 NA  4 NA 11.1 19  7  7   4  6  8  1   0
#> 84                 Texas  9  0 NA NA NA  9.2  8  2  2   4  6  8  1   1
#> 85               Oakland 16  0  1  1  8 15.2 14  6  6   8  8 16  1   1
#> 86       Chicago,Toronto  2  0 NA NA NA  2.1  0  0  0   2  2  2  0   0
#> 87            Washington  2  0 NA NA  1  7.1  5  2  2   0  0  8  1   0
#> 88            Pittsburgh  8  8  7  1 NA 55.1 45 13  9  12 12 58  3   4
#> 89               Arizona  7  6 NA  3 NA 31.2 44 26 26   6  8 14  9   0
#> 90             Tampa Bay  8  8  1  3 NA 39.0 53 26 26  14 16 25  9   1
#> 91              New York  8  8  4  4 NA 45.2 58 36 33   6  8 34  8   1
#> 92                 Miami  1  0 NA NA NA  1.0  0  0  0   0  0  0  0   0
#> 93            Cincinnati  2  0 NA NA NA  2.0  0  0  0   0  0  0  0   0
#> 94               Toronto  2  2  1  1 NA 11.0 14  4  4   0  0  5  0   0
#> 95          Philadelphia  2  2 NA  1 NA 10.2 12  6  2   2  2  8  2   0
#> 96                 Miami  1  1 NA NA NA  5.1  5  4  4   3  3  4  2   0
#> 97             Milwaukee 15  0 NA NA NA 17.0 18  7  7   3  3 19  2   0
#> 98           Los Angeles  2  0 NA NA NA  3.1  3  4  4   3  3  5  0   0
#> 99             Milwaukee  1  1 NA  1 NA  7.0  4  1  1   2  2  6  0   0
#> 100            Cleveland  2  0 NA NA NA  1.1  1  0  0   0  0  0  0   0
#> 101           Cincinnati  6  6  1  1 NA 39.1 33 14 13   8  8 39  3   1
#> 102              Atlanta 16  0  1  2 NA 15.0 14 10 10   8  8 17  2   0
#> 103              Chicago  8  8  2  4 NA 50.1 56 27 26  15 15 34  6   0
#> 104          Kansas City 17  0  1  1  2 17.0  9  1  1   7  8 20  0   0
#> 105         Philadelphia 20  0 NA  1 NA 19.2 22 15 15   3  3 21  5   1
#> 106             Colorado  7  7  4  1 NA 40.2 35 20 20  22 22 34  5   1
#> 107              Arizona  8  8  3  1 NA 52.0 56 32 31  14 14 45  8   2
#> 108              Houston  2  0 NA NA NA  3.0  3  2  2   1  1  2  1   1
#> 109             New York  8  8  4  2 NA 55.2 33 13 12   9 10 61  3   1
#> 110              Toronto 14  0  2 NA NA 13.2  5  2  2   5  5 16  2   0
#> 111              Arizona 15  0  2 NA  1 18.0 21  9  8   8  8 16  3   0
#> 112           Cincinnati  8  8  3  2 NA 46.2 50 22 22  16 18 29  2   1
#> 113            San Diego  7  7  1  4 NA 41.1 52 22 22   6  7 24  5   4
#> 114                Texas  5  1 NA  1 NA  8.1 12  5  5   2  2  4  1   1
#> 115           Cincinnati 14  0  1  1 NA 13.1 14  8  8   2  3 13  2   0
#> 116              Toronto  8  8  2  3 NA 50.0 51 31 30  18 18 38 10   1
#> 117         Philadelphia 13  0  1 NA NA 11.2 10  7  6   7  7 16  1   1
#> 118              Oakland  1  0 NA NA NA  1.0  1  0  0   0  0  2  0   0
#> 119            Baltimore  5  0 NA NA NA  7.2  5  3  3   6  6  7  1   0
#> 120            Minnesota 15  0  2 NA NA  8.1 11 11 11   4  6  4  1   1
#> 121          Kansas City  2  2 NA  2 NA  8.2  9 10 10  10 10  7  2   0
#> 122              Chicago 17  0  1  1  1 14.1 17  9  9   6  8 14  5   1
#> 123                Miami 17  0  1  3 NA 12.2 12  7  7   3  3 13  2   0
#> 124                Miami 18  0  2  2 NA 15.0 13  9  9   7  7 12  1   1
#> 125                Texas  9  0 NA NA NA  5.2  6  4  4   6  6  5  1   0
#> 126              Seattle  8  8  4  3 NA 49.2 46 20 19  16 16 36  6   4
#> 127            Tampa Bay  1  0 NA NA NA  1.0  3  1  1   0  0  0  1   0
#> 128             New York  8  8  3  2 NA 42.1 56 27 27  11 11 29  3   1
#> 129              Toronto  8  8  4  2 NA 49.0 43 24 23  18 18 39  6   1
#> 130              Atlanta  5  0 NA NA NA  1.2  4  1  1   1  1  3  1   0
#> 131             New York 15  0  2 NA  7 16.2 13  2  2   7  7 15  1   0
#> 132              Detroit  1  1 NA  1 NA  5.0  9  7  7   1  1  1  2   0
#> 133              Seattle  8  0 NA  2 NA 10.2 13 12 10   5  5  9  2   1
#> 134              Houston  4  4  2  1 NA 25.2 25 12 11   8  8 19  1   0
#> 135              Houston  1  0 NA NA NA  1.0  1  0  0   0  0  1  0   0
#> 136                Texas  5  0 NA  1  2  5.1  7  4  4   2  4  1  1   0
#> 137              Houston 16  0  1  1 NA 14.1  5  3  3   7  7 22  0   0
#> 138            Minnesota  9  0  1 NA NA 10.0  5  0  0   1  1 11  0   0
#> 139            Milwaukee  8  8  2  3 NA 44.1 50 19 19  13 14 40  3   2
#> 140          Kansas City  3  0 NA NA NA  6.2  3  1  1   3  3  5  0   0
#> 141           Washington  2  2 NA  2 NA  7.1 17 12 12   1  1  4  3   0
#> 142             Colorado  3  0 NA NA NA  7.1 11  6  6   3  3  3  0   0
#> 143                Miami  2  0 NA NA NA  3.2  4  0  0   1  1  1  0   1
#> 144              Atlanta  7  7  1  2 NA 41.2 51 27 24  11 11 39  7   2
#> 145              Toronto  2  0 NA  1 NA  0.1  4  3  3   0  0  1  0   1
#> 146         Philadelphia  1  0 NA NA NA  2.0  1  2  2   3  3  1  1   1
#> 147            Tampa Bay  1  0 NA NA NA  1.0  3  2  2   0  0  0  1   1
#> 148          Kansas City 10  0  1 NA NA 10.0 10  0  0   6  8  5  0   0
#> 149                Texas 15  0 NA NA NA 10.1  9  7  7   3  3  5  1   2
#> 150          Los Angeles  8  8  1  5 NA 47.0 58 31 28  14 15 23  4   3
#> 151             Colorado 16  0 NA  1 NA 14.2 18  8  8   1  2 18  2   0
#> 152            Tampa Bay  9  0 NA NA NA  9.2 10  6  6   4  4  8  2   0
#> 153                Texas  2  0 NA NA NA  1.2  2  3  3   0  0  1  1   1
#> 154              Seattle 15  0 NA NA NA 11.1  3  2  2   1  1  6  2   0
#> 155                Texas  7  7  4  1 NA 44.0 36 13 11  11 11 33  2   0
#> 156            San Diego 18  0 NA NA NA 15.1 17 10  9   8  8  8  3   0
#> 157            St. Louis  6  6  2  3 NA 41.0 30  9  8   7  7 28  2   0
#> 158            Baltimore  1  0 NA NA NA  2.1  1  1  1   4  4  2  0   0
#> 159         Philadelphia 17  0  1  1  1 16.2 16 10 10   6  8 17  4   0
#> 160          Los Angeles 19  0  1  2 NA 16.2 19 12 10   5  5 22  5   1
#> 161            Milwaukee  8  7  2  4 NA 46.1 55 32 28  11 13 35  7   0
#> 162            Baltimore  1  1 NA NA NA  5.0  4  2  2   1  1  1  0   1
#> 163             New York  3  2 NA  1 NA  9.1 22 16 13   2  3  5  2   1
#> 164            Tampa Bay 16  1 NA  1  1 14.2  6  2  2   4  5 13  1   0
#> 165            Minnesota  7  7  1  3 NA 44.2 42 20 18   8 10 38  7   2
#> 166         Philadelphia 16  0  1 NA NA 15.2 13  5  5   4  5 19  1   0
#> 167             New York 16  0  1 NA NA 16.2 11  3  3   3  4 13  0   0
#> 168             New York 13  0 NA NA NA 16.0 12  5  5   3  5 16  1   2
#> 169            Milwaukee  4  0 NA NA NA  2.1  1  0  0   1  1  1  0   0
#> 170            Tampa Bay 14  0 NA  1 NA 12.1 15  4  4   2  2 10  2   0
#> 171         Philadelphia 18  0 NA  1 NA 23.0 21  4  3   5  6 15  0   0
#> 172                Texas  4  4  2  1 NA 30.0 18  3  3  12 12 10  1   1
#> 173           Washington  7  7  1  2 NA 37.1 45 25 25  16 17 29  2   2
#> 174            Baltimore  6  6  2  2 NA 37.2 29 14 13   9 10 30  7   3
#> 175         Philadelphia  3  3  1  1 NA 12.0 18 10 10   1  1 13  2   3
#> 176              Detroit 15  0 NA NA NA 10.1 18 14 14   8  9  6  2   2
#> 177          Los Angeles  3  0 NA NA NA  3.0  3  0  0   0  0  3  0   0
#> 178           Washington  9  0  1  1 NA  7.0 14  8  7   5  6  6  0   1
#> 179            Minnesota  9  1 NA NA NA 19.0 25 13 10   2  2 14  5   0
#> 180              Oakland  6  6  2  2 NA 39.2 37 10 10   9  9 30  5   1
#> 181              Oakland  8  8  4  3 NA 52.2 38 15 13  10 10 46  3   0
#> 182              Detroit  7  7  1  4 NA 34.0 41 25 23   6  8 23  7   4
#> 183              Houston 14  0 NA  1 10 13.0 15  8  8   4  4 18  2   0
#> 184          Los Angeles  8  8 NA  2 NA 54.1 47 12 12   8  8 49  5   2
#> 185              Atlanta 17  0  2  1 11 16.1 15  5  3   4  4 17  1   0
#> 186              Chicago 19  0  1  1 NA 15.1 11  2  2   9  9 22  1   0
#> 187              Seattle  1  0 NA NA NA  2.1  0  0  0   0  0  2  0   0
#> 188              Chicago  2  0 NA NA NA  3.0  5  1  1   0  1  2  0   0
#> 189            Tampa Bay  2  0 NA NA NA  4.0  5  3  3   2  2  5  1   0
#> 190          Kansas City  7  7  3  2 NA 35.0 42 21 21   8  9 19  6   3
#> 191            Cleveland 17  0 NA  1 NA 12.1 12  6  6   2  3 11  1   0
#> 192              Oakland  8  8  4  2 NA 52.1 40 18 15  15 15 33  2   5
#> 193             Colorado  5  5  2  2 NA 30.2 35 19 18   3  4 28  7   1
#> 194         Philadelphia  7  7  3  2 NA 50.1 39 13 13  10 10 57  3   3
#> 195              Chicago  7  7  2  1 NA 45.2 33 17 12   7  9 51  4   3
#> 196                Miami 12  3  1  1 NA 21.2 28 18 18   6  7 15  0   0
#> 197           Cincinnati  1  0 NA NA NA  3.0  2  0  0   1  1  3  0   1
#> 198              Seattle  7  7 NA  2 NA 35.1 43 17 17   9 10 25  3   1
#> 199         Philadelphia  8  8  1  6 NA 49.2 44 28 24  15 16 36  8   4
#> 200              Detroit 17  0  2  1 NA 16.1 10  4  3   3  4 17  0   0
#> 201                Miami  8  8  2  3 NA 50.1 43 20 20   9 10 40  8   3
#> 202            St. Louis  9  0 NA  1 NA  9.2 15  8  7   7  7  4  2   0
#> 203              Houston 15  0  2 NA NA 16.2  7  2  2   3  3 16  2   0
#> 204             New York  7  7  2  3 NA 47.0 40 20 20   8  9 50  8   1
#> 205          Los Angeles 14  0  1  1 NA  8.1 11  6  5   4  4  4  1   0
#> 206             Colorado  5  0 NA NA NA  5.0  5  1  1   1  1  5  1   0
#> 207              Arizona  7  7  3  1 NA 40.0 33 20 20  12 13 30  8   1
#> 208               Boston  3  0 NA NA NA  5.0  2  0  0   1  1  0  0   0
#> 209              Chicago  8  8  2  2 NA 48.0 50 21 19   8  8 40  4   1
#> 210              Toronto 15  0  2 NA NA 18.2 16  5  5   2  3 20  2   1
#> 211              Arizona  5  0 NA NA NA  3.1  3  1  0   3  3  2  0   0
#> 212              Seattle  8  8  5  3 NA 49.0 37 23 23  20 20 43  8   1
#> 213              Houston  7  5  1  2 NA 35.2 45 24 22  12 12 17  5   0
#> 214          Kansas City 15  0  1 NA NA 13.2 10  3  3   3  3 12  1   0
#> 215        San Francisco  8  8  5  2 NA 47.1 42 23 22  10 12 44  4   6
#> 216           Washington  6  0 NA NA NA 12.0 14  5  5   4  4  9  2   1
#> 217          Kansas City 10  0 NA NA NA  9.1 17  8  8   2  2  9  2   0
#> 218          Kansas City 12  0  1 NA  8 11.1  8  6  6  10 10 13  1   0
#> 219           Cincinnati 19  0  3 NA NA 18.0  9  2  0   6  6 12  0   0
#> 220          Los Angeles 14  0  1 NA NA 11.2  5  1  0   2  3  8  0   1
#> 221              Arizona 16  1  1  1  1 19.2 14  5  3   8  8 23  1   0
#> 222        San Francisco  8  8  4  3 NA 45.2 51 23 23  12 12 24  5   3
#> 223          Los Angeles  2  0 NA NA NA  2.0  4  2  2   0  0  2  0   0
#> 224           Pittsburgh 16  0  1 NA NA 14.0 13  6  6   3  4  7  1   2
#> 225            Minnesota  7  7  3  2 NA 46.2 48 20 20   5  5 23  7   1
#> 226            Baltimore 15  0  1 NA NA 16.2 18  5  5   0  1 10  1   0
#> 227              Toronto  7  7  3  1 NA 41.1 41 19 19   8  8 38  5   1
#> 228          Los Angeles  2  0 NA NA NA  4.0  4  1  1   3  3  3  0   0
#> 229           Cincinnati  5  3  1  1 NA 19.2 22 11 11   7  7 21  1   2
#> 230              Chicago 10  0  1 NA NA 15.1 11  3  2   5  6 10  0   0
#> 231          Los Angeles 12  0  2 NA  9 12.0  4  1  1   0  0 22  1   0
#> 232           Washington 12  0 NA  1 NA 11.0 10  4  4   2  3  4  0   0
#> 233            Milwaukee 17  0  2 NA NA 16.2 18  6  6   3  5 22  1   0
#> 234              Chicago 10  0  1  2 NA  9.0 13  9  9   3  4  8  2   0
#> 235            Tampa Bay 19  0  1  3  4 17.1 15  7  5  11 11 14  1   0
#> 236            Baltimore  8  8  3  1 NA 47.2 55 21 20  14 14 51  4   2
#> 237              Atlanta 19  0  1  1  2 18.0 14  5  4   4  6 14  0   1
#> 238             New York  1  0 NA NA NA  0.2  0  0  0   1  1  0  0   1
#> 239           Washington  2  1 NA  2 NA 10.2 16 10 10   3  3  5  0   0
#> 240            Milwaukee  3  3  2  1 NA 18.0 18  7  7   4  4 14  1   2
#> 241             Colorado  9  0 NA NA NA 10.1  6  1  1   6  6 11  1   0
#> 242            Tampa Bay  7  7  2  2 NA 38.2 42 15 14  12 12 34  3   0
#> 243              Oakland  7  7  1  3 NA 36.2 34 17 12  17 17 33  2   0
#> 244                Texas 17  0  2  3  1 15.0 14  5  5   6  6 16  1   0
#> 245            San Diego 15  0 NA NA NA 17.2 15  4  4   2  2 19  2   0
#> 246               Boston  7  7  1  2 NA 37.0 42 19 18  14 14 24  2   0
#> 247             Colorado  8  8  1  5 NA 51.2 49 25 24  16 17 21  8   2
#> 248            San Diego  8  8  2  4 NA 43.1 46 27 27  13 13 44  9   3
#> 249          Los Angeles  8  8  4  2 NA 54.1 38 19 18  15 16 71  4   2
#> 250              Houston  8  8  4  3 NA 55.2 46 21 20  15 15 46  6   0
#> 251            San Diego 15  0  1  1  8 14.1 12  4  3   6  7 23  1   0
#> 252            Milwaukee  6  0 NA  1 NA  6.0  9  4  4   4  4  6  0   0
#> 253                Texas  2  2  1 NA NA  7.1 11  8  7   3  3  6  3   0
#> 254            Cleveland  8  8  3  4 NA 59.0 46 17 17  11 11 71  4   3
#> 255            Milwaukee 14  0 NA NA NA 15.0 14  5  5   6  6 15  2   0
#> 256                Miami  8  7  3  1 NA 46.0 39 17 14  19 19 34  4   1
#> 257        San Francisco 18  0 NA NA NA 16.2 11  5  4   1  1 11  2   0
#> 258              Detroit  4  0 NA  1 NA  3.2  8  6  6   3  3  4  2   1
#> 259            St. Louis  8  8  4  3 NA 50.1 51 22 20  11 12 38  5   1
#> 260           Pittsburgh  1  0 NA NA NA  1.0  1  0  0   0  0  1  0   0
#> 261                Miami  5  5  1  1 NA 28.0 35 16 16   6  6 30  2   0
#> 262               Boston 20  0 NA  1  1 14.2  9  4  4   9  9 18  1   0
#> 263           Cincinnati  8  8  2  3 NA 46.0 55 29 28  15 16 28  8   2
#> 264             New York 15  0 NA  1 NA 10.2 10  3  3   7  7 13  0   1
#> 265            Cleveland  1  0 NA NA NA  0.2  2  1  1   0  0  0  0   0
#> 266              Oakland  5  0 NA NA NA  5.1  7  2  2   0  0  7  1   0
#> 267      Arizona,Seattle  3  0 NA  2 NA  4.0  7  6  6   2  3  3  0   0
#> 268              Chicago  8  8  2  3 NA 49.2 52 21 20  15 15 47  8   0
#> 269                Texas  8  8  5  1 NA 50.1 56 31 29   7  8 39  5   4
#> 270          Los Angeles 17  0  2  1 NA 12.0 11  6  6   3  5 14  0   0
#> 271        San Francisco  7  7  4  1 NA 37.1 35 20 19  21 21 29  6   1
#> 272             New York  7  0 NA NA NA  7.0  5  4  4   4  4  8  3   0
#> 273           Pittsburgh  8  8  3  4 NA 49.2 39 20 20  12 12 61  4   1
#> 274           Pittsburgh  4  0 NA  1 NA  6.1  7  3  3   2  4  7  1   0
#> 275              Detroit  3  3 NA  3 NA 14.2 21 12 12   5  5  5  3   0
#> 276           Pittsburgh  8  8  2  1 NA 44.1 46 24 23  19 19 34  5   2
#> 277             Colorado 17  0 NA  1 NA 11.2 11  6  6   8  8 14  2   2
#> 278            Milwaukee  8  8  1  5 NA 46.0 53 29 29   9 10 34  7   1
#> 279        San Francisco 18  0 NA NA NA 11.1  2  0  0   5  5  9  0   0
#> 280           Cincinnati  8  6  1  1 NA 37.2 31 18 16  22 24 21  4   1
#> 281              Toronto 18  0  1  3 NA 13.2 13  9  8   1  1 13  1   1
#> 282              Seattle 17  0 NA NA NA 19.0 15  4  2   8  9 22  0   1
#> 283             Colorado  4  4 NA  2 NA 12.1 16 11 10   2  2  9  0   2
#> 284            St. Louis  6  6  3  1 NA 38.0 37 11 10  13 13 35  3   0
#> 285            St. Louis  4  4  2 NA NA 18.2 22 11 10   5  5 21  5   0
#> 286        San Francisco  8  0 NA NA NA 10.2 15  8  7   3  3  7  2   1
#> 287          Kansas City 15  0 NA  1 NA 12.2 11  3  3   5  5 12  1   1
#> 288            St. Louis 15  0  1 NA  1 11.1 14  4  4   1  2  8  0   0
#> 289            Cleveland  1  0 NA NA NA  1.0  0  0  0   1  1  2  0   0
#> 290            Cleveland  6  6  3  2 NA 30.0 29 20 20   7  8 26  8   1
#> 291              Atlanta  1  0 NA NA NA  1.0  4  3  3   0  0  0  0   0
#> 292           Cincinnati  3  3 NA  2 NA 12.1 21 14 12   3  4  9  3   0
#> 293              Arizona  1  0 NA NA NA  1.1  2  0  0   0  0  0  0   0
#> 294             New York  3  0 NA  1 NA  3.1  8  5  5   0  0  2  1   0
#> 295              Atlanta  6  0  1  2 NA  4.0  8  7  7   3  3  2  1   0
#> 296            St. Louis  7  7  4  2 NA 45.1 33  7  6  17 17 50  2   2
#> 297                Texas  8  8  3  3 NA 47.2 49 22 20  18 20 35  6   8
#> 298        Atlanta,Miami 16  0  2  1 NA 15.1 13  7  5   3  5 12  3   1
#> 299               Boston  1  1 NA  1 NA  2.1  6  6  6   1  1  1  2   0
#> 300           Cincinnati 15  0 NA  1 NA 15.0 15  7  6   3  3  9  1   1
#> 301            Baltimore 11  0 NA NA NA 11.2 11  4  4   5  5 15  1   2
#> 302            San Diego 19  0  3 NA NA 19.0  8  1  1   3  3 19  0   1
#> 303            Minnesota  8  8  2  3 NA 47.2 50 21 21  10 10 45  5   2
#> 304                Miami  9  0 NA NA NA 10.1 14  6  5   5  6  5  0   0
#> 305            San Diego  6  0 NA NA NA  6.2 17 16 14   3  3  7  1   0
#> 306            Cleveland 15  0  2  1 NA 17.0 13  6  6   5  6 22  2   1
#> 307              Houston  7  7  3  2 NA 40.1 28 14 11  12 13 46  1   3
#> 308            Baltimore  9  0 NA  1 NA  6.1 11  6  2   6  7  8  1   0
#> 309            Tampa Bay 14  0 NA NA  3 13.0  9  4  3   1  1 21  1   0
#> 310         Philadelphia  4  0 NA  1 NA  9.1 15 11  9   4  4 12  6   0
#> 311              Houston  8  8  3  3 NA 46.2 58 34 34  14 14 37  8   6
#> 312              Chicago  1  0 NA NA NA  2.0  0  0  0   0  0  0  0   0
#> 313           Pittsburgh 20  0 NA NA 17 20.1 15  2  1   1  2 11  1   0
#> 314               Boston  7  7  5  2 NA 43.1 45 18 17  11 11 29  4   1
#> 315             New York 11  0 NA  1  4 10.2  5  3  3   2  2 17  1   2
#> 316             Colorado  2  0 NA NA NA  1.2  2  0  0   0  0  2  0   0
#> 317              Atlanta  8  8  1  2 NA 52.1 43 16 13  13 15 34  1   2
#> 318            Minnesota  3  3  1 NA NA 19.0 17  7  5   2  2 13  3   0
#> 319             New York  1  0 NA NA  1  3.0  4  1  1   0  0  2  0   0
#> 320              Seattle  4  4  1  2 NA 26.1 22  8  8   8  8 12  1   0
#> 321          Kansas City 15  0 NA NA NA 15.0  9  3  3   4  4  9  1   0
#> 322          Los Angeles  5  0 NA  1 NA  3.0  6  5  4   3  3  4  0   0
#> 323             New York  1  0 NA NA NA  0.2  3  5  5   3  3  0  1   0
#> 324                Miami 13  0 NA NA NA 11.2 17  4  4   3  3 10  1   0
#> 325           Pittsburgh  5  5  5 NA NA 33.1 25  8  6   8  8 15  1   0
#> 326           Cincinnati  3  3  1  1 NA 11.2 11  6  6   5  5  6  2   0
#> 327              Chicago 16  0  3  1  1 11.2 11  3  3   1  4  9  1   0
#> 328              Oakland  6  0  1  1 NA  5.2  5  3  1   1  1  6  1   0
#> 329            Cleveland  1  0 NA NA NA  0.1  2  5  0   1  1  0  1   1
#> 330            Milwaukee  8  8  2  5 NA 47.1 55 28 27  18 20 35  6   5
#> 331              Detroit 13  0  1  1 NA 10.1 15 10 10   5  7  4  2   3
#> 332              Houston 15  0 NA NA  1 12.2  8  3  3   3  3 11  1   0
#> 333          Los Angeles 12  1 NA  1  1 13.2 14  4  4   5  6 13  0   0
#> 334                Miami  1  1  1 NA NA  7.0  4  0  0   2  2  2  0   0
#> 335             New York  7  7 NA  5 NA 40.0 49 33 28  12 13 30  7   2
#> 336              Chicago  6  1 NA  1 NA 16.1 25 15 14   6  6  6  3   0
#> 337            Minnesota  4  4  3 NA NA 19.2 24 10  7   4  4 21  1   0
#> 338            Baltimore  4  4  1  2 NA 20.0 23 11 11   7  7 16  3   2
#> 339      Arizona,Seattle  9  0 NA  1 NA 20.1 14  4  4   5  6 26  2   0
#> 340            Baltimore 16  0  3 NA  2 16.1  9  2  2   5  5 24  1   2
#> 341              Oakland  4  0 NA NA NA  4.0  6  2  2   4  4  2  0   0
#> 342         Philadelphia  8  8  1  4 NA 45.1 54 24 24  10 14 20  7   3
#> 343             Colorado 17  0  1  1  1 18.2 15 10 10   7  7 10  4   2
#> 344              Houston  5  5  2  1 NA 26.1 29 10  8   9  9 18  0   0
#> 345            Tampa Bay  6  6  1  3 NA 36.0 35 12 11   7  7 31  5   1
#> 346               Boston 14  0 NA NA NA 14.1 15  6  6   5  5  9  2   0
#> 347                Texas  8  0  1 NA  1  7.2  7  3  3   3  4  9  2   0
#> 348              Toronto 18  0  1  2 NA 15.2 11  7  6   3  4 19  1   1
#> 349              Oakland  7  0 NA  1 NA 11.0 16 10 10   2  3  8  2   1
#> 350         Philadelphia 14  0  1  1  7 14.1 10  3  1   3  4 17  0   1
#> 351             New York  4  0 NA NA  1  3.1  3  0  0   1  1  2  0   0
#> 352           Cincinnati  9  0 NA NA NA  7.1  2  2  2   0  0  6  1   0
#> 353              Seattle  4  4  3  1 NA 24.2 20  6  5  11 11 15  2   0
#> 354            Minnesota  7  7  2  3 NA 44.1 49 17 16   9 10 21  3   1
#> 355            Milwaukee  3  3 NA  1 NA 15.0 16  7  7   5  6 11  2   1
#> 356              Arizona 15  0 NA  1 NA 10.1  6  6  4   3  3 11  0   2
#> 357              Atlanta  9  7  4 NA  1 45.0 39 11 10  19 19 36  3   4
#> 358            Minnesota 16  0 NA  1 12 16.0 15  3  3   4  4 15  2   0
#> 359          Los Angeles  6  0 NA NA NA  2.1  5  2  1   3  4  5  0   0
#> 360        San Francisco  8  1  1  1 NA 21.0 17  7  7   1  1 13  3   1
#> 361              Chicago 17  0  1  1  1 11.2 15  4  4   1  2 11  0   0
#> 362                Miami  7  7  2  3 NA 41.2 47 25 22   8  8 31  5   2
#> 363                Texas  1  0 NA  1 NA  1.0  1  1  1   0  0  0  1   0
#> 364             New York  5  0 NA NA NA  7.1  6  2  2   0  0  5  1   0
#> 365             New York  7  7  4  3 NA 42.0 47 23 19   8  8 49  6   0
#> 366          Kansas City  2  1 NA  1 NA  8.2 14  7  7   3  3  5  2   0
#> 367              Oakland 11  2  1 NA NA 18.0  9  5  5   9 10 11  0   1
#> 368               Boston  8  8  1  6 NA 47.2 57 35 35  11 11 32  7   4
#> 369            Minnesota 18  0  2  2 NA 16.0 15  7  7  10 10 11  0   0
#> 370              Detroit  7  7  3  1 NA 51.0 40 13 10  10 11 55  4   1
#> 371              Chicago 12  0 NA  1 NA 11.2  5  3  2   4  5 21  1   0
#> 372            San Diego 11  0 NA  1 NA 10.2 16  5  5   4  6  7  0   0
#> 373              Houston 14  0  1  1  1 13.0 13  8  8   3  3  8  3   0
#> 374              Chicago  7  7  2  4 NA 46.1 48 18 17  14 15 39  3   2
#> 375            Cleveland  1  0 NA NA NA  0.2  1  2  0   1  1  0  0   0
#> 376            Tampa Bay  9  8  6  1 NA 44.1 31 13 13  14 14 37  3   7
#> 377              Arizona 12  0  1  1 NA 15.1 15  7  7   2  4 11  1   0
#> 378             New York  3  0 NA NA NA  3.0  6  5  5   4  4  2  0   1
#> 379                Miami 16  0 NA  1  9 15.2  9  3  3   3  3 21  1   0
#> 380          Los Angeles 15  0  1 NA NA 11.2 15  4  4   2  2  8  1   1
#> 381                Texas  1  1 NA NA NA  6.2  5  0  0   2  2  4  0   0
#> 382          Los Angeles  7  0  2  1 NA  7.0 11  5  5   1  1  9  2   0
#> 383              Arizona  4  4  1  2 NA 24.1 18  6  5   9  9 18  2   1
#> 384              Toronto  1  1 NA NA NA  4.0  4  3  3   1  1  3  1   0
#> 385              Arizona 16  0  2  1  2 16.1 19 12 12   5  8 12  2   0
#> 386          Los Angeles  8  8  5  3 NA 47.2 42 25 22  18 18 40  5   2
#> 387            Tampa Bay  3  0  1 NA NA  4.1  3  2  2   2  2  1  1   0
#> 388                Miami  3  0 NA NA NA  7.0  4  3  2   4  4  8  2   0
#> 389           Washington  7  5  3 NA NA 35.0 32 14 14   6  7 21  8   1
#> 390             Colorado  6  0 NA NA NA  7.0  8  3  3   2  2  3  0   0
#> 391              Chicago 16  0  2  2  9 17.2 14  9  8   3  4 20  3   1
#> 392           Washington  1  0 NA NA NA  1.0  1  0  0   0  0  1  0   0
#> 393             New York 18  0  1  1 NA 14.1 14  9  8   5  6 10  1   0
#> 394              Seattle 16  0  1  2  6 15.2 18 12 12   6  7 10  3   2
#> 395              Chicago  7  7  2  1 NA 38.0 39 19 17  20 20 36  1   1
#> 396               Boston  5  5  3  1 NA 31.2 22 11 11  11 11 27  2   2
#> 397              Oakland 16  0 NA  1 NA 12.1 15  9  9   4  4 12  0   1
#> 398            Milwaukee 14  0 NA NA  7 14.0  6  1  1   4  5 15  0   0
#> 399          Los Angeles  6  0 NA NA NA  3.1  7  2  2   0  0  2  0   0
#> 400                Texas  8  8  3  1 NA 47.1 45 18 17  14 14 34  3   2
#> 401            Baltimore 12  0  2 NA NA 17.0 11  2  2   4  5 19  1   1
#> 402             New York  9  0 NA NA NA 14.1 26 22 17   9  9 14  2   1
#> 403            Tampa Bay  4  0 NA NA NA  8.1 13  9  8   6  6  7  0   0
#> 404        San Francisco 18  0 NA  2 NA 12.1 14  7  7   2  3 16  1   1
#> 405              Chicago 16  0  2  1  6 15.1 13  4  3   3  3 11  1   0
#> 406            St. Louis 15  0  1 NA 10 15.2 10  0  0   4  5 15  0   0
#> 407         Philadelphia  1  0 NA NA NA  2.0  7  5  5   1  1  0  1   1
#> 408           Washington  3  3  2  1 NA 20.1 19  6  6   2  2 23  0   0
#> 409               Boston  5  0 NA NA NA  8.2  9  3  3   3  3  8  1   1
#> 410            San Diego  8  8  3  4 NA 49.0 50 21 18  18 20 50  0   3
#> 411              Chicago 15  0  1  1 NA 11.2 15  8  8   8  8 13  3   0
#> 412             Colorado  5  4  2  2 NA 28.0 35 16 16  10 11 20  4   0
#> 413              Chicago 20  0 NA  1 NA 12.0 13  4  3   5  6  6  1   0
#> 414              Detroit  4  2  1  1 NA 19.1 16  8  7   6  6 10  4   0
#> 415            Cleveland 18  0 NA  2 NA  7.1  5  2  1   2  2  9  1   1
#> 416             New York  8  8  3  2 NA 45.0 54 27 26   7  7 45  9   1
#> 417          Los Angeles 19  0  1 NA NA 15.0 15  8  8   2  4 17  1   0
#> 418            Cleveland  8  8  3  1 NA 47.1 44 21 18  13 14 54  7   1
#> 419              Chicago  8  8  4  2 NA 61.1 29 10  9  10 10 93  4   2
#> 420              Chicago  8  8  2  2 NA 55.1 65 33 30   9  9 48  6   3
#> 421              Toronto  5  5  2  2 NA 33.2 33 14 13  12 12 18  4   1
#> 422              Detroit  7  7  3  3 NA 48.0 39 21 21  13 13 46  7   0
#> 423          Los Angeles  8  7  2  2 NA 46.1 42 16 15  11 12 47  8   3
#> 424 Los Angeles,New York  8  0 NA NA NA 10.1 10  7  7   3  3 12  3   0
#> 425           Pittsburgh 11  0 NA  1 NA 11.0 11  8  3   4  5 10  1   1
#> 426                Texas 16  0  3 NA NA 16.1 12  7  7   5  7 13  4   0
#> 427           Washington  8  8  6  2 NA 59.2 33 10 10   9  9 74  4   1
#> 428              Chicago  3  0  1 NA NA  1.2  2  0  0   0  0  0  0   0
#> 429              Toronto  4  0 NA NA NA  7.2  5  4  2   1  1  4  0   1
#> 430              Oakland 19  0  2  1 NA 19.0 22 10  9   1  1 19  4   1
#> 431            Cleveland 15  0 NA NA  1 13.2  9  6  3   4  4 11  2   0
#> 432            San Diego  8  8  3  1 NA 51.1 49 20 19  17 17 54  5   3
#> 433          Los Angeles  7  7  2  2 NA 41.1 38 22 19   8  9 35  5   0
#> 434             New York 14  0  3 NA NA 15.1  9  3  3   4  4 16  1   0
#> 435            St. Louis 19  0  2 NA  2 17.2 13  4  4   5  5 27  3   1
#> 436              Detroit  7  7  3  3 NA 42.2 37 20 15  16 17 35  4   0
#> 437              Houston 15  0 NA  3 NA 11.2 11  6  6   2  2 17  3   0
#> 438              Seattle 16  0 NA NA  5 15.0 10  3  3   1  1 19  1   2
#> 439          Los Angeles 15  0  2  1 NA 14.2  5  4  4   3  3 14  0   0
#> 440            Milwaukee 17  0  1 NA NA 15.1 10  4  1   4  4 19  0   1
#> 441            St. Louis  4  0 NA NA NA  6.2  5  2  2   2  2  6  0   0
#> 442           Washington  3  0  1 NA NA  4.0  6  5  5   1  2  2  2   0
#> 443              Detroit 12  0 NA NA  5 11.2 13  5  5   2  2  9  4   0
#> 444            Minnesota  5  0 NA NA NA  5.1  8  3  2   1  1  4  1   0
#> 445           Washington 14  0 NA NA 12 13.0  9  4  4   1  1 22  1   1
#> 446           Washington  4  4  1  2 NA 13.0 22 18 16   4  4 13  5   0
#> 447          Los Angeles 15  0 NA  1 11 15.1  7  2  2   2  2 16  2   0
#> 448        San Francisco 13  0 NA NA NA 14.1 11  4  4   3  3 18  0   0
#> 449              Chicago 18  0  1  1  2 18.1  6  6  4   4  6 19  2   1
#> 450              Atlanta  3  2 NA  2 NA 12.1 10 12 12   5  6  9  3   0
#> 451              Seattle  1  0 NA NA NA  1.0  1  0  0   0  0  0  0   0
#> 452             New York  8  8  2  4 NA 44.2 49 24 20   9 10 48  4   2
#> 453             New York  3  3  2  1 NA 21.0 17  4  4   0  0 21  2   0
#> 454               Boston 18  0 NA  2 NA 16.0 14  8  7   1  2 17  0   0
#> 455              Atlanta  7  7  1  2 NA 43.2 47 28 26  12 13 30  6   1
#> 456              Toronto 11  0 NA NA NA 12.2  9  4  4   1  1  9  2   2
#> 457              Houston 12  0  1  1 NA  6.2  4  1  1   4  4  9  0   0
#> 458            San Diego 17  0  2  2 NA 15.0 16  8  8   5  7 13  2   0
#> 459  Atlanta,Los Angeles  2  0 NA  1 NA  5.0  7  5  5   4  4  3  1   0
#> 460            Minnesota 17  0  1  2 NA  7.1 12  7  7   5  5  2  0   0
#> 461           Washington 12  0 NA NA NA  8.1  7  1  1   1  2  5  0   0
#> 462            Baltimore  7  7  3  3 NA 39.1 39 23 22  18 18 26  3   2
#> 463                Texas 19  0  1  1 10 20.0 18  2  2   1  4 16  1   2
#> 464            Minnesota 10  0 NA NA NA  7.0  9  3  3   3  3  7  1   1
#> 465             New York 15  0 NA NA NA 14.2 14  4  4   8  9 16  2   1
#> 466             New York 17  0  1  2 NA 17.2 20 10 10   3  6 10  1   0
#> 467           Washington 14  0  1 NA NA 20.0 21 10  8   4  5 25  2   0
#> 468            St. Louis  2  0 NA  1 NA  1.0  1  1  1   2  2  1  0   0
#> 469               Boston 17  0 NA  2  9 14.2 13  9  5   5  6 14  1   0
#> 470                Miami  5  5  1  3 NA 29.1 29 12 12  10 10 13  2   2
#> 471          Kansas City  3  3  2  1 NA 16.0 17  4  4   1  1 11  1   1
#> 472           Washington  6  0 NA NA NA  7.0  2  1  1   2  2  8  0   0
#> 473              Atlanta  2  0 NA NA NA  1.0  5  3  3   0  0  1  1   0
#> 474              Houston  2  2 NA NA NA  9.2 10  5  5   6  6 12  1   0
#> 475              Oakland  4  0 NA NA NA  5.2  1  0  0   2  2  4  0   0
#> 476          Kansas City  6  6  1  4 NA 34.0 33 16 16   7  8 30  3   1
#> 477              Detroit  2  2 NA  1 NA 11.2 13  8  8   3  3  4  4   0
#> 478             New York  2  0 NA NA NA  2.2  2  1  1   0  1  4  0   0
#> 479            St. Louis  8  0 NA  1 NA 13.0 16  2  2   3  3 10  1   0
#> 480           Cincinnati  3  0 NA  1 NA  6.1  6  2  1   0  0  5  1   0
#> 481            San Diego  4  0 NA NA NA  3.2  2  0  0   1  1  7  0   0
#> 482        San Francisco  8  8  4  3 NA 47.2 39 17 16  18 18 34  2   1
#> 483          Kansas City  8  8  5  1 NA 46.1 45 20 20  17 17 32  4   3
#> 484            St. Louis  7  7  4  2 NA 44.2 36 15 14  10 11 43  4   1
#> 485              Chicago  6  6  1  1 NA 29.1 27 12 12  10 11 30  4   1
#> 486            Milwaukee  1  1 NA NA NA  3.2  9  5  5   1  1  2  1   0
#> 487              Seattle  8  8  4  3 NA 50.2 44 18 18  12 12 50  7   0
#> 488             New York  7  7  3  3 NA 46.0 36 16 15  12 12 34  6   2
#> 489           Pittsburgh 17  0 NA NA NA 17.1 14  4  3   6  6 13  0   1
#> 490          Los Angeles  8  8  3  4 NA 53.1 53 28 27  11 11 28  8   1
#> 491              Chicago  7  0 NA NA NA  8.2  9  1  0   1  2 10  0   1
#> 492            Cleveland 11  0  1 NA NA 13.2 11  5  4   4  5 13  1   2
#> 493              Arizona  2  2  1  1 NA 11.0  7  8  8   7  8  6  2   0
#> 494          Los Angeles  1  0 NA NA NA  2.0  1  0  0   1  1  0  0   0
#> 495             New York  1  1 NA  1 NA  1.2  2  3  3   2  2  0  0   0
#> 496              Seattle 14  0  1  1 NA 15.2 19  9  7   7  7 17  0   0
#> 497         Philadelphia  8  8  1  5 NA 37.0 54 32 31  12 12 21  9   4
#> 498              Detroit 17  1  1  2 NA 25.2 18  4  4   3  4 15  0   1
#> 499          Los Angeles  8  8  4  3 NA 53.0 45 23 23  17 17 54  6   4
#> 500              Detroit  1  0 NA NA NA  1.0  2  1  1   0  0  0  1   0
#> 501             New York 17  0  1 NA NA 13.2 12  4  4   6  6 10  0   0
#> 502            Baltimore  5  1  1  1 NA 17.0 18  4  4   3  3  4  0   1
#> 503              Atlanta  1  1  1 NA NA  8.0  6  1  1   0  0  2  0   1
#> 504              Atlanta  7  7  3  2 NA 46.0 47 17 14  11 12 36  4   1
#> 505              Chicago 12  1  1  1  1 17.1 17  8  8   4  5 18  4   0
#> 506           Pittsburgh  6  1 NA  2 NA 14.0 16  5  3   1  1  7  1   1
#> 507            Baltimore  6  6  2  3 NA 29.2 30 17 17   9 11 19  4   4
#> 508               Boston 10  4  2  2 NA 38.1 35 21 18  10 10 23  6   0
#> 509          Kansas City  7  7  4  2 NA 42.0 34 12 11  11 11 23  3   0
#> 510              Arizona 17  0 NA  1 10 16.2 12  4  3   4  5  9  2   0
#> 511           Washington  8  8  3  3 NA 49.1 58 19 19  13 13 38  3   0
#>       ERA  AB X1B X2B X3B IBB GDP SF SB CS PO  BF Pit  Str  StL  StS
#> 1    2.45  13   0   2   0   1   0  0  2  0  0  17  62 0.61 0.15 0.13
#> 2    4.50  29   2   1   0   0   0  0  0  0  0  30 114 0.65 0.13 0.16
#> 3    2.70  24   4   2   0   0   2  0  0  0  0  26  86 0.64 0.13 0.16
#> 4    6.75  22   2   3   0   0   0  0  1  0  0  26 109 0.50 0.16 0.07
#> 5    5.52  61  15   1   0   1   2  0  2  0  0  67 248 0.59 0.20 0.05
#> 6    0.93  69   9   4   0   1   0  1  0  0  0  77 313 0.63 0.20 0.14
#> 7    1.37  65   8   3   0   0   2  0  1  0  0  72 279 0.63 0.22 0.13
#> 8    8.71  43  12   3   0   1   1  1  1  0  0  52 170 0.61 0.15 0.05
#> 9    4.05  50   7   4   0   0   2  0  0  0  0  54 198 0.66 0.18 0.11
#> 10   0.00   1   0   0   0   0   0  0  0  0  0   1   2 0.50 0.00 0.00
#> 11   3.38 169  38   2   0   1   5  1  0  3  3 188 695 0.62 0.18 0.08
#> 12   2.72 165  31  10   1   0   6  0  1  1  0 178 688 0.65 0.18 0.08
#> 13   0.66  52  11   0   0   1   0  1  0  0  0  55 200 0.65 0.19 0.08
#> 14   3.55  53  15   2   0   1   3  0  2  0  0  61 227 0.62 0.14 0.11
#> 15   1.69 196  37   4   0   0   3  0  4  3  0 206 789 0.69 0.18 0.14
#> 16   3.40 164  27   6   1   0   6  1  4  3  0 179 707 0.63 0.20 0.11
#> 17  10.13  36   7   0   0   0   0  0  0  0  0  39 151 0.67 0.17 0.04
#> 18  13.50  16   1   2   0   0   1  0  0  0  0  23 104 0.54 0.14 0.04
#> 19   3.68  57  10   2   0   1   2  0  1  0  0  61 222 0.66 0.13 0.11
#> 20   1.15  55   5   2   0   0   1  0  2  0  0  60 256 0.61 0.16 0.08
#> 21   2.70  66  12   3   0   1   0  0  2  0  0  71 262 0.65 0.22 0.05
#> 22   0.00   8   1   1   0   0   0  0  0  1  0   8  41 0.63 0.15 0.15
#> 23   4.61  59  12   4   0   0   1  0  2  0  0  64 235 0.63 0.13 0.09
#> 24   7.27  35   8   1   0   0   1  0  2  1  0  38 158 0.66 0.20 0.10
#> 25   6.59  52   7   6   2   0   3  1  0  0  0  59 237 0.63 0.16 0.06
#> 26   2.79  37   5   1   0   1   0  0  0  0  0  39 167 0.65 0.14 0.09
#> 27   2.47 167  21   6   0   0   0  0  1  2  0 189 738 0.60 0.17 0.10
#> 28   6.00  26   6   3   1   0   1  0  1  0  0  31 101 0.63 0.12 0.03
#> 29   4.91  46  12   4   0   2   0  0  1  1  1  55 216 0.59 0.18 0.06
#> 30   4.30  54   9   2   0   2   1  1  0  1  1  63 230 0.61 0.15 0.07
#> 31   0.00  13   0   1   0   0   0  0  0  0  0  14  56 0.59 0.11 0.11
#> 32   2.40  58  15   2   1   2   2  1  1  1  0  67 243 0.68 0.19 0.11
#> 33   2.70  23   3   1   0   0   1  0  0  1  0  29 116 0.57 0.14 0.16
#> 34   1.93  48   2   1   0   0   0  0  0  0  0  52 215 0.65 0.10 0.14
#> 35   5.82  68  11   6   1   0   0  0  0  0  0  72 282 0.68 0.19 0.08
#> 36   0.52  55   4   0   0   1   1  1  3  0  0  64 278 0.60 0.23 0.17
#> 37  11.88  38   8   2   1   0   0  0  1  0  0  40 140 0.69 0.19 0.09
#> 38   3.44 191  28  15   0   0   3  0  3  0  0 209 792 0.64 0.15 0.11
#> 39   5.73  48  10   4   0   0   0  0  0  0  0  50 167 0.68 0.21 0.03
#> 40   1.80  76  11   6   1   1   1  1  0  0  0  81 306 0.67 0.16 0.11
#> 41   2.08  77  10   3   0   1   1  1  1  0  0  85 313 0.64 0.21 0.13
#> 42   3.02 171  28   8   0   0   6  0  4  4  0 191 730 0.61 0.23 0.08
#> 43   3.68  57  12   1   0   1   0  1  1  2  0  65 276 0.68 0.21 0.13
#> 44   1.96  67   9   4   0   3   2  0  0  1  0  73 244 0.65 0.16 0.09
#> 45   2.45  63   7   1   0   0   0  1  0  0  0  69 285 0.66 0.15 0.17
#> 46  10.91  70  19   4   1   1   3  1  3  0  0  84 314 0.58 0.18 0.06
#> 47   5.79  34  10   2   0   1   1  3  1  0  0  43 142 0.62 0.11 0.09
#> 48   2.16  64  15   1   0   0   2  0  0  0  0  66 218 0.74 0.17 0.19
#> 49   3.38   9   3   0   0   0   0  0  0  2  0  11  42 0.64 0.19 0.12
#> 50   0.00   3   0   0   0   0   0  0  0  0  0   3  11 0.73 0.00 0.09
#> 51   2.89  33   3   1   1   0   0  0  3  0  0  41 175 0.58 0.10 0.17
#> 52   7.80  59  11   2   3   1   3  0  0  0  0  66 263 0.64 0.14 0.11
#> 53   2.00  31   0   4   0   0   0  0  1  0  0  37 137 0.63 0.15 0.07
#> 54   2.63 205  34  13   0   0   9  0  1  0  0 220 803 0.67 0.17 0.11
#> 55   2.73 213  25  15   0   3   8  3  0  1  1 229 777 0.65 0.18 0.06
#> 56   2.90 183  30   6   1   1   3  1  0  1  1 196 723 0.67 0.15 0.13
#> 57   3.52  30   5   2   0   0   0  0  0  1  0  32 133 0.68 0.14 0.23
#> 58   2.32 207  41  11   1   0   5  0  5  5  1 221 785 0.66 0.22 0.10
#> 59   6.26  99  23   7   1   1   3  0  1  1  1 114 405 0.62 0.16 0.07
#> 60   0.00   2   0   0   0   0   1  0  0  0  0   3  15 0.47 0.07 0.13
#> 61   5.59  38   9   3   0   0   3  0  2  0  0  41 149 0.65 0.19 0.09
#> 62   1.65  53   3   2   0   0   0  1  1  0  0  61 235 0.64 0.14 0.11
#> 63   1.17  51   6   1   0   0   1  1  4  0  0  59 222 0.68 0.18 0.25
#> 64   5.14  88  19   5   0   0   1  0  0  0  0  97 380 0.67 0.16 0.13
#> 65   0.00   1   0   0   0   0   0  0  0  0  0   2  10 0.60 0.20 0.00
#> 66   2.53  40   7   3   0   0   0  1  0  1  0  49 178 0.65 0.15 0.12
#> 67   4.15 204  38  10   2   0   3  2  2  3  0 217 816 0.68 0.17 0.11
#> 68   4.05  28   6   3   0   1   1  0  1  0  0  34 147 0.61 0.18 0.07
#> 69   5.06 187  33  13   3   1   8  2  2  2  0 208 799 0.65 0.18 0.07
#> 70   2.70  51   6   2   1   0   1  0  0  0  0  57 220 0.64 0.21 0.12
#> 71   3.86  10   4   0   0   0   0  0  0  0  0  13  50 0.60 0.12 0.06
#> 72   4.85  50   8   2   0   0   0  0  0  0  0  55 239 0.63 0.15 0.16
#> 73   3.86  50  13   2   0   0   0  0  0  0  0  55 223 0.64 0.18 0.11
#> 74   2.70  64  12   4   0   0   2  1  0  0  0  69 258 0.66 0.18 0.06
#> 75   1.80  38   7   4   0   0   0  0  0  0  1  41 165 0.59 0.16 0.11
#> 76   3.57  66  13   1   0   0   0  2  1  0  0  81 346 0.63 0.14 0.15
#> 77   6.75   6   1   1   0   0   0  0  0  0  0   8  32 0.53 0.22 0.09
#> 78   2.85 199  38   9   0   0   6  2  2  3  0 212 822 0.64 0.19 0.09
#> 79  11.57  13   4   1   0   0   1  0  0  0  0  13  35 0.71 0.17 0.03
#> 80   3.18 171  25  13   1   0   4  3  1  1  0 181 667 0.69 0.17 0.09
#> 81   2.25  26   3   1   0   0   2  0  0  0  0  30 103 0.60 0.21 0.05
#> 82   4.61  50   7   4   1   1   0  2  2  0  0  65 259 0.59 0.15 0.11
#> 83   5.56  48  14   4   0   2   2  1  3  0  0  56 205 0.61 0.19 0.08
#> 84   1.86  35   5   1   1   2   1  1  0  0  0  43 174 0.62 0.17 0.10
#> 85   3.45  61  11   2   0   0   0  0  1  1  0  70 291 0.63 0.14 0.12
#> 86   0.00   7   0   0   0   0   0  0  0  0  0   9  43 0.63 0.19 0.05
#> 87   2.45  27   4   0   0   0   0  0  0  0  0  27 102 0.65 0.16 0.10
#> 88   1.46 197  38   4   0   0   8  3  8  3  0 218 805 0.66 0.20 0.10
#> 89   7.39 132  21  13   1   2   4  2  2  0  0 143 579 0.62 0.15 0.07
#> 90   6.00 158  37   6   1   2   5  1  2  4  0 177 662 0.62 0.18 0.07
#> 91   6.50 193  36  14   0   2   1  1  1  0  0 206 697 0.70 0.21 0.06
#> 92   0.00   3   0   0   0   0   0  0  0  0  0   3   6 0.67 0.17 0.00
#> 93   0.00   6   0   0   0   0   0  0  0  0  0   6  27 0.63 0.19 0.15
#> 94   3.27  45   9   4   1   0   1  1  0  0  0  46 169 0.67 0.21 0.07
#> 95   1.69  43   7   3   0   0   1  0  0  0  0  46 167 0.65 0.16 0.09
#> 96   6.75  20   2   0   1   0   0  0  0  0  0  23  91 0.60 0.13 0.11
#> 97   3.71  63  15   1   0   0   3  0  0  0  0  69 267 0.66 0.16 0.13
#> 98  10.80  13   1   2   0   0   0  0  0  0  0  16  65 0.62 0.14 0.08
#> 99   1.29  21   3   1   0   0   3  0  0  0  0  23  87 0.64 0.15 0.09
#> 100  0.00   4   1   0   0   0   1  0  0  0  0   4  11 0.73 0.27 0.00
#> 101  2.97 149  19  11   0   0   2  0  0  0  0 159 613 0.68 0.14 0.11
#> 102  6.00  57   9   2   1   0   2  0  0  0  0  65 255 0.59 0.14 0.13
#> 103  4.65 196  34  14   2   0   5  1  1  1  0 215 822 0.67 0.17 0.09
#> 104  0.53  59   7   1   1   1   1  0  2  1  0  67 286 0.63 0.17 0.11
#> 105  6.86  77  10   7   0   0   1  1  2  1  0  83 335 0.63 0.13 0.13
#> 106  4.43 149  21   8   1   0   5  2  5  2  1 175 692 0.61 0.13 0.12
#> 107  5.37 203  37  11   0   0   8  0  3  1  0 222 762 0.68 0.17 0.14
#> 108  6.00  12   1   0   1   0   0  0  0  0  0  14  64 0.64 0.17 0.14
#> 109  1.94 191  27   3   0   1   4  2  0  2  0 208 819 0.68 0.17 0.13
#> 110  1.32  44   1   2   0   0   0  1  0  1  0  50 213 0.64 0.14 0.15
#> 111  4.00  74  17   1   0   0   1  0  1  1  0  82 337 0.62 0.15 0.14
#> 112  4.24 181  31  16   1   2   5  1  3  0  0 203 783 0.64 0.17 0.08
#> 113  4.79 166  38   8   1   1   3  1  2  3  0 181 674 0.65 0.20 0.06
#> 114  5.40  34   8   3   0   0   0  0  0  2  3  38 159 0.62 0.15 0.05
#> 115  5.40  53   6   5   1   1   0  0  0  0  0  57 215 0.63 0.14 0.13
#> 116  5.40 196  29  11   1   0   1  3  1  1  0 219 829 0.65 0.16 0.10
#> 117  4.63  44   6   3   0   0   1  0  1  1  1  52 223 0.63 0.16 0.13
#> 118  0.00   4   1   0   0   0   0  0  0  0  0   4  14 0.64 0.29 0.21
#> 119  3.52  26   3   1   0   0   0  1  0  0  0  33 136 0.60 0.14 0.13
#> 120 11.88  33   7   3   0   2   1  0  0  0  0  41 141 0.56 0.13 0.08
#> 121 10.38  36   5   2   0   0   0  1  1  0  0  47 210 0.57 0.19 0.02
#> 122  5.65  55   9   3   0   2   4  0  2  0  0  64 256 0.62 0.13 0.11
#> 123  4.97  50   7   3   0   0   0  0  0  0  0  53 186 0.65 0.17 0.14
#> 124  5.40  56  10   2   0   0   1  0  2  0  0  65 243 0.60 0.12 0.12
#> 125  6.35  21   4   1   0   0   1  1  2  0  0  28 127 0.54 0.19 0.09
#> 126  3.44 188  32   8   0   0   5  1  3  1  0 210 747 0.63 0.17 0.10
#> 127  9.00   6   2   0   0   0   0  0  0  0  0   6  17 0.65 0.18 0.00
#> 128  5.74 179  45   7   1   0   1  1  0  1  0 192 697 0.65 0.18 0.08
#> 129  4.22 187  28   7   2   0   1  1  4  1  1 207 823 0.65 0.16 0.10
#> 130  5.40   9   0   3   0   0   0  0  0  0  0  10  37 0.65 0.22 0.14
#> 131  1.08  60  11   1   0   0   2  1  0  1  0  68 282 0.62 0.14 0.15
#> 132 12.60  22   5   1   1   0   1  0  0  0  0  24  84 0.70 0.18 0.08
#> 133  8.44  44   7   4   0   0   1  0  1  0  0  50 191 0.62 0.14 0.09
#> 134  3.86  96  18   5   1   0   5  2  4  0  0 106 405 0.63 0.18 0.07
#> 135  0.00   4   0   1   0   0   0  0  0  0  0   4  17 0.65 0.24 0.12
#> 136  6.75  23   4   2   0   2   0  0  0  0  0  27  97 0.56 0.14 0.08
#> 137  1.88  45   3   2   0   0   0  1  1  2  0  53 235 0.63 0.13 0.13
#> 138  0.00  34   4   1   0   0   0  0  0  0  0  35 138 0.68 0.20 0.10
#> 139  3.86 177  32  12   3   1   3  2  3  1  0 195 756 0.64 0.15 0.08
#> 140  1.35  22   3   0   0   0   2  0  2  0  0  25 109 0.60 0.18 0.06
#> 141 14.73  38  12   1   1   0   1  0  0  1  0  39 128 0.65 0.17 0.07
#> 142  7.36  31   7   3   1   0   1  0  0  0  0  35 110 0.64 0.18 0.07
#> 143  0.00  14   4   0   0   0   1  0  0  0  0  16  55 0.65 0.13 0.09
#> 144  5.18 169  34   9   1   0   2  2  1  1  0 186 659 0.67 0.16 0.09
#> 145 81.00   5   3   1   0   0   0  0  0  0  0   6  18 0.67 0.22 0.06
#> 146  9.00   6   0   0   0   0   0  1  0  0  0  11  48 0.52 0.19 0.00
#> 147 18.00   6   1   1   0   0   0  0  0  0  0   7  17 0.71 0.29 0.06
#> 148  0.00  37   8   2   0   2   1  0  0  0  0  46 212 0.59 0.16 0.05
#> 149  6.10  37   6   2   0   0   1  1  2  0  0  43 166 0.64 0.17 0.09
#> 150  5.36 183  46   8   0   1   6  3  2  2  0 208 713 0.63 0.17 0.11
#> 151  4.91  58  14   2   0   1   1  1  0  0  0  63 236 0.64 0.13 0.15
#> 152  5.59  38   6   2   0   0   0  2  1  0  0  44 165 0.70 0.21 0.08
#> 153 16.20   7   1   0   0   0   0  0  0  0  0   8  26 0.65 0.23 0.04
#> 154  1.59  38   1   0   0   0   0  0  0  0  0  39 149 0.64 0.15 0.09
#> 155  2.25 162  27   7   0   0   6  2  0  1  0 175 709 0.61 0.19 0.06
#> 156  5.28  55  12   2   0   0   4  2  1  1  1  67 237 0.63 0.18 0.11
#> 157  1.76 143  23   5   0   0   9  0  0  2  1 151 554 0.66 0.18 0.08
#> 158  3.86   6   0   1   0   0   1  0  1  0  0  10  44 0.52 0.16 0.14
#> 159  5.40  62  10   2   0   2   0  1  3  0  0  74 283 0.60 0.16 0.14
#> 160  5.40  67  12   2   0   0   0  1  0  0  0  74 253 0.70 0.17 0.17
#> 161  5.44 190  36  12   0   2   3  0  1  1  0 206 710 0.67 0.17 0.08
#> 162  3.60  18   3   1   0   0   0  1  0  0  0  21  91 0.64 0.19 0.07
#> 163 12.54  49  16   4   0   1   1  1  2  0  0  55 177 0.70 0.17 0.08
#> 164  1.23  48   3   2   0   1   0  0  1  1  0  54 216 0.63 0.17 0.10
#> 165  3.63 168  26   9   0   2   8  1  1  1  0 182 722 0.65 0.17 0.10
#> 166  2.87  58   9   3   0   1   1  1  1  0  0  64 255 0.66 0.09 0.21
#> 167  1.62  58   7   4   0   1   1  1  0  1  0  63 214 0.68 0.19 0.14
#> 168  2.81  56   8   3   0   2   1  3  4  0  0  66 244 0.60 0.14 0.13
#> 169  0.00   6   1   0   0   0   0  0  0  2  0   7  24 0.58 0.17 0.13
#> 170  2.92  49  10   3   0   0   0  1  0  0  0  52 207 0.69 0.14 0.11
#> 171  1.17  87  19   2   0   1   1  0  1  2  0  94 336 0.63 0.19 0.08
#> 172  0.90 104  15   2   0   0   2  0  1  0  0 117 428 0.62 0.16 0.06
#> 173  6.03 147  35   6   2   1   6  3  3  1  0 169 643 0.63 0.20 0.09
#> 174  3.11 138  19   3   0   1   3  0  0  0  0 152 587 0.67 0.16 0.12
#> 175  7.50  51  16   0   0   0   0  1  1  0  0  56 217 0.67 0.19 0.11
#> 176 12.19  47  11   4   1   1   0  0  0  0  0  58 201 0.55 0.14 0.10
#> 177  0.00  12   3   0   0   0   0  0  0  0  0  12  43 0.67 0.28 0.09
#> 178  9.00  32  10   4   0   1   1  1  1  0  0  40 145 0.57 0.14 0.07
#> 179  4.74  76  15   3   2   0   2  1  1  0  0  80 311 0.66 0.16 0.13
#> 180  2.27 150  27   5   0   0   1  1  2  1  0 162 599 0.64 0.18 0.08
#> 181  2.22 193  30   4   1   0   3  1  3  0  0 204 787 0.64 0.19 0.10
#> 182  6.09 139  23   7   4   2   1  1  2  1  0 152 553 0.66 0.16 0.07
#> 183  5.54  54  10   3   0   0   2  0  3  0  0  58 212 0.69 0.17 0.17
#> 184  1.99 198  36   6   0   0   5  1  1  1  0 211 759 0.65 0.16 0.13
#> 185  1.65  64   8   6   0   0   0  1  0  0  1  69 285 0.66 0.14 0.14
#> 186  1.17  55   9   1   0   0   3  0  1  0  0  64 279 0.62 0.19 0.15
#> 187  0.00   7   0   0   0   0   0  0  0  0  0   7  27 0.74 0.19 0.15
#> 188  3.00  12   5   0   0   1   0  0  1  0  0  13  45 0.64 0.16 0.09
#> 189  6.75  16   3   1   0   0   0  1  1  0  0  19  76 0.68 0.20 0.07
#> 190  5.40 141  31   4   1   1   2  1  1  0  0 155 616 0.64 0.16 0.08
#> 191  4.38  47  10   1   0   1   0  1  0  1  1  51 175 0.65 0.11 0.08
#> 192  2.58 194  34   4   0   0   8  1  0  2  0 216 811 0.63 0.17 0.08
#> 193  5.28 121  22   6   0   1   3  1  3  0  0 128 465 0.65 0.13 0.12
#> 194  2.32 186  30   6   0   0   2  0  8  0  0 201 762 0.69 0.14 0.16
#> 195  2.36 168  19   9   1   2   4  2  5  1  0 182 714 0.68 0.18 0.14
#> 196  7.48  91  17  11   0   1   2  0  3  0  0  98 371 0.59 0.12 0.08
#> 197  0.00  11   1   1   0   0   0  0  1  0  0  13  59 0.61 0.15 0.14
#> 198  4.33 142  31   8   1   1   4  0  3  3  0 153 628 0.62 0.16 0.07
#> 199  4.35 185  25   9   2   1   5  0  1  1  0 208 810 0.64 0.15 0.08
#> 200  1.65  57   5   2   3   1   1  0  0  0  0  61 211 0.68 0.17 0.13
#> 201  3.58 187  23  11   1   1   2  2  0  0  0 204 766 0.65 0.19 0.07
#> 202  6.52  40  13   0   0   0   1  0  1  0  0  48 171 0.61 0.16 0.09
#> 203  1.08  55   4   1   0   0   2  0  1  0  0  58 261 0.64 0.18 0.08
#> 204  3.83 173  23   8   1   1   5  1  2  0  0 185 688 0.70 0.19 0.13
#> 205  5.40  35   9   1   0   0   1  1  0  0  0  40 158 0.65 0.15 0.07
#> 206  1.80  19   4   0   0   0   1  0  0  0  0  20  59 0.73 0.20 0.08
#> 207  4.50 148  21   4   0   1   2  1  3  0  0 166 666 0.62 0.16 0.10
#> 208  0.00  15   2   0   0   0   2  0  0  0  0  16  49 0.67 0.12 0.04
#> 209  3.56 186  34  11   1   0   5  0  2  1  0 196 699 0.66 0.22 0.08
#> 210  2.41  71  12   2   0   1   0  0  2  1  0  75 281 0.70 0.17 0.11
#> 211  0.00  11   2   1   0   0   1  0  0  0  0  14  52 0.56 0.13 0.06
#> 212  4.22 176  25   3   1   0   6  1  3  0  0 199 730 0.65 0.19 0.10
#> 213  5.55 142  26  14   0   0   6  1  7  2  0 155 574 0.64 0.19 0.06
#> 214  1.98  49   9   0   0   0   1  1  1  0  0  53 186 0.64 0.14 0.12
#> 215  4.18 179  30   7   1   2   4  0  1  0  0 198 726 0.63 0.21 0.09
#> 216  3.75  46   9   2   1   0   4  0  2  0  0  51 198 0.59 0.16 0.10
#> 217  7.71  42  11   3   1   0   1  1  1  0  0  45 178 0.66 0.17 0.10
#> 218  4.76  40   5   2   0   0   1  0  0  0  0  51 200 0.58 0.18 0.15
#> 219  0.00  61   7   1   1   0   1  0  0  0  0  68 271 0.63 0.15 0.08
#> 220  0.00  41   5   0   0   1   1  0  1  0  0  45 169 0.62 0.17 0.11
#> 221  1.37  70  11   1   1   0   1  1  0  1  0  79 314 0.60 0.14 0.14
#> 222  4.53 180  40   5   1   0   8  0  1  0  0 195 701 0.65 0.18 0.10
#> 223  9.00   9   4   0   0   0   0  0  0  0  0   9  26 0.81 0.19 0.15
#> 224  3.86  50  11   1   0   1   1  1  0  0  0  59 188 0.65 0.11 0.11
#> 225  3.86 182  33   8   0   0   2  0  3  0  0 189 642 0.72 0.19 0.05
#> 226  2.70  62  16   1   0   1   3  1  5  1  0  64 216 0.69 0.15 0.11
#> 227  4.14 157  26  10   0   0   6  1  3  1  0 167 643 0.64 0.18 0.09
#> 228  2.25  16   3   1   0   0   0  0  0  0  0  19  80 0.56 0.16 0.04
#> 229  5.03  81  15   5   1   0   1  0  1  0  0  90 351 0.68 0.18 0.12
#> 230  1.17  53   8   3   0   1   2  0  3  1  0  59 219 0.65 0.14 0.11
#> 231  0.75  41   2   1   0   0   0  0  1  0  0  41 155 0.78 0.19 0.21
#> 232  3.27  41   7   2   1   1   1  0  0  0  0  45 161 0.64 0.16 0.05
#> 233  3.24  66  16   1   0   2   1  0  1  0  0  71 270 0.63 0.15 0.11
#> 234  9.00  37  10   1   0   1   0  0  0  0  0  44 164 0.60 0.17 0.08
#> 235  2.60  62  13   0   1   0   2  2  1  2  0  76 302 0.62 0.22 0.07
#> 236  3.78 193  40  10   1   0   3  0  9  1  0 210 769 0.63 0.19 0.10
#> 237  2.00  66  11   3   0   2   2  1  5  0  0  74 268 0.66 0.18 0.07
#> 238  0.00   2   0   0   0   0   0  0  0  0  0   4  17 0.41 0.18 0.06
#> 239  8.44  46  12   4   0   0   0  1  0  0  0  51 174 0.68 0.17 0.09
#> 240  3.50  69  12   3   2   0   0  1  4  2  1  76 296 0.65 0.19 0.06
#> 241  0.87  34   4   1   0   0   3  0  0  0  0  40 164 0.60 0.17 0.13
#> 242  3.26 155  31   8   0   0   2  0  3  1  0 168 649 0.63 0.20 0.08
#> 243  2.95 138  23   8   1   0   4  1  4  0  1 158 630 0.65 0.15 0.11
#> 244  3.00  59  12   1   0   0   2  0  1  0  0  65 261 0.67 0.14 0.15
#> 245  2.04  64  12   1   0   0   0  0  0  2  1  66 226 0.72 0.22 0.13
#> 246  4.38 149  31   7   2   0   2  2  1  1  0 165 654 0.62 0.19 0.06
#> 247  4.18 197  31   9   1   1   1  0  3  3  1 218 758 0.64 0.18 0.07
#> 248  5.61 168  27   9   1   0   3  0  5  1  0 186 795 0.65 0.18 0.10
#> 249  2.98 197  29   5   0   1   2  0  3  2  1 216 839 0.65 0.17 0.15
#> 250  3.23 208  38   2   0   0   4  0  1  0  0 223 845 0.63 0.18 0.10
#> 251  1.88  53   7   4   0   1   1  0  2  1  0  60 248 0.62 0.22 0.15
#> 252  6.00  25   7   2   0   0   2  0  0  0  0  29 122 0.61 0.15 0.10
#> 253  8.59  35   4   4   0   0   0  0  0  0  0  38 147 0.67 0.18 0.05
#> 254  2.59 211  29  12   1   0   6  1  4  1  0 226 838 0.70 0.16 0.15
#> 255  3.00  57   8   3   1   0   1  0  0  0  0  63 253 0.64 0.17 0.11
#> 256  2.74 167  26   9   0   0   5  2  1  1  0 190 723 0.63 0.16 0.07
#> 257  2.16  59   7   2   0   0   0  1  1  1  0  62 206 0.71 0.17 0.13
#> 258 14.73  17   6   0   0   0   1  0  0  0  0  22  84 0.65 0.15 0.10
#> 259  3.58 195  34  10   2   1   5  2  1  0  0 212 721 0.69 0.19 0.08
#> 260  0.00   4   0   1   0   0   0  0  0  0  0   4  18 0.67 0.11 0.11
#> 261  5.14 117  23   9   1   0   0  1  3  0  0 125 455 0.69 0.16 0.12
#> 262  2.45  52   4   4   0   0   0  0  1  0  0  61 220 0.58 0.20 0.10
#> 263  5.48 184  37  10   0   1   6  0  0  1  0 205 720 0.65 0.17 0.06
#> 264  2.53  40   8   2   0   0   1  0  4  1  0  48 212 0.59 0.18 0.10
#> 265 13.50   4   1   1   0   0   0  0  0  0  0   4  11 0.82 0.36 0.00
#> 266  3.38  23   6   0   0   0   0  0  1  0  0  23  83 0.63 0.10 0.16
#> 267 13.50  18   5   2   0   1   0  1  0  0  0  22  80 0.61 0.14 0.09
#> 268  3.62 194  31  11   2   0   4  0 11  1  0 210 810 0.63 0.18 0.11
#> 269  5.19 201  34  17   0   1   1  5  5  0  0 219 775 0.67 0.21 0.08
#> 270  4.50  47   8   3   0   2   0  0  1  0  0  52 213 0.61 0.16 0.12
#> 271  4.58 143  21   8   0   0   2  1  8  0  0 166 671 0.60 0.17 0.11
#> 272  5.14  24   1   1   0   0   1  1  0  0  0  29 116 0.66 0.15 0.16
#> 273  3.62 185  29   6   0   0   6  0  2  1  0 198 766 0.63 0.18 0.15
#> 274  4.26  24   4   2   0   2   2  0  0  0  0  28 107 0.53 0.11 0.10
#> 275  7.36  61  13   3   2   0   2  1  1  1  1  67 232 0.59 0.10 0.09
#> 276  4.67 170  35   6   0   0   3  3  4  0  0 196 779 0.60 0.17 0.09
#> 277  4.63  46   8   1   0   0   0  1  0  0  0  57 232 0.62 0.17 0.15
#> 278  5.67 182  39   7   0   1   2  2  2  1  0 198 742 0.66 0.18 0.10
#> 279  0.00  34   1   1   0   0   2  0  0  0  0  39 151 0.63 0.18 0.11
#> 280  3.82 133  18   9   0   2   6  0  0  1  1 159 611 0.58 0.15 0.06
#> 281  5.27  53   9   2   1   0   1  0  0  0  0  56 203 0.68 0.22 0.09
#> 282  0.95  68  13   2   0   1   2  0  1  1  0  78 297 0.64 0.21 0.12
#> 283  7.30  52  13   3   0   0   3  0  1  0  0  56 214 0.64 0.13 0.05
#> 284  2.37 148  29   5   0   0   1  0  2  0  0 162 653 0.63 0.15 0.10
#> 285  4.82  78  12   4   1   0   1  0  1  0  0  84 336 0.68 0.20 0.10
#> 286  5.91  48   7   5   1   0   0  0  1  0  0  52 169 0.62 0.13 0.07
#> 287  2.13  48   9   1   0   0   0  1  0  0  0  55 184 0.65 0.15 0.11
#> 288  3.18  43  10   4   0   1   2  0  0  0  0  48 164 0.70 0.23 0.07
#> 289  0.00   3   0   0   0   0   0  0  0  0  0   4  21 0.52 0.14 0.10
#> 290  6.00 114  15   5   1   1   1  0  0  2  0 123 494 0.62 0.19 0.12
#> 291 27.00   7   1   1   2   0   0  0  0  0  0   7  18 0.67 0.06 0.06
#> 292  8.76  56  16   2   0   1   0  2  5  1  0  63 220 0.64 0.13 0.10
#> 293  0.00   5   2   0   0   0   1  0  0  0  0   5  20 0.60 0.15 0.10
#> 294 13.50  18   6   1   0   0   0  0  0  0  0  18  80 0.60 0.19 0.04
#> 295 15.75  17   4   3   0   0   0  0  1  0  0  21  76 0.61 0.16 0.07
#> 296  1.19 162  25   5   1   0   5  2  0  0  1 183 715 0.64 0.17 0.10
#> 297  3.78 187  28  10   5   2   5  2  2  2  3 217 817 0.62 0.19 0.08
#> 298  2.93  56   8   2   0   2   2  1  1  1  0  63 228 0.59 0.11 0.10
#> 299 23.14  13   2   1   1   0   0  0  0  0  0  14  57 0.53 0.23 0.05
#> 300  3.60  56   9   5   0   0   3  1  1  0  1  61 223 0.66 0.18 0.06
#> 301  3.09  45   8   2   0   0   0  0  0  1  1  52 199 0.63 0.14 0.14
#> 302  0.47  66   7   1   0   0   0  1  0  0  0  71 268 0.65 0.14 0.15
#> 303  3.97 185  30  11   4   0   2  1  5  2  0 199 744 0.66 0.18 0.10
#> 304  4.35  42   8   5   1   1   0  2  1  0  0  50 183 0.58 0.11 0.08
#> 305 18.90  36  12   4   0   0   1  0  0  0  0  39 138 0.66 0.18 0.06
#> 306  3.18  63   8   2   1   1   1  1  0  0  0  71 293 0.67 0.18 0.09
#> 307  2.45 148  19   7   1   1   3  0  0  0  0 164 674 0.62 0.20 0.12
#> 308  2.84  31  10   0   0   1   1  0  0  0  0  38 145 0.59 0.14 0.08
#> 309  2.08  48   7   1   0   0   0  0  0  0  0  49 195 0.69 0.16 0.16
#> 310  8.68  43   8   1   0   0   2  0  0  0  0  47 181 0.62 0.12 0.14
#> 311  6.56 185  38  11   1   0   5  3  3  1  0 211 797 0.63 0.15 0.10
#> 312  0.00   6   0   0   0   0   0  0  0  0  0   6  19 0.68 0.21 0.05
#> 313  0.44  76  14   0   0   1   1  0  0  1  0  78 263 0.72 0.19 0.09
#> 314  3.53 172  31   8   2   0   4  0  0  0  0 184 678 0.63 0.19 0.08
#> 315  2.53  38   4   0   0   0   0  0  0  0  0  42 176 0.64 0.24 0.16
#> 316  0.00   7   1   1   0   0   0  0  1  0  0   7  26 0.73 0.08 0.19
#> 317  2.24 192  33   7   2   2   4  1  4  2  2 212 778 0.66 0.17 0.08
#> 318  2.37  72  12   2   0   0   1  0  3  1  1  74 300 0.65 0.20 0.07
#> 319  3.00  13   3   1   0   0   0  0  0  0  0  13  51 0.67 0.08 0.04
#> 320  2.73  98  15   6   0   0   2  0  0  1  1 106 389 0.62 0.16 0.07
#> 321  1.80  52   5   2   1   0   0  0  0  0  0  57 209 0.58 0.18 0.07
#> 322 12.00  15   5   1   0   0   0  0  0  0  0  18  72 0.53 0.17 0.06
#> 323 67.50   5   2   0   0   0   0  0  0  0  0   8  25 0.48 0.16 0.04
#> 324  3.09  50  14   2   0   0   2  0  2  0  0  54 181 0.66 0.09 0.18
#> 325  1.62 121  20   4   0   0   7  0  3  0  0 129 450 0.67 0.19 0.07
#> 326  4.63  44   8   1   0   0   0  1  2  0  0  50 185 0.63 0.19 0.05
#> 327  2.31  45   8   2   0   3   0  0  1  1  0  50 221 0.67 0.15 0.05
#> 328  1.59  20   4   0   0   0   1  1  2  1  0  22  89 0.72 0.10 0.22
#> 329  0.00   4   1   0   0   0   0  0  0  0  0   6  21 0.43 0.00 0.05
#> 330  5.13 185  39  10   0   2   5  4  5  2  0 215 775 0.63 0.17 0.10
#> 331  8.71  42  11   2   0   2   0  1  0  0  0  54 193 0.62 0.10 0.07
#> 332  2.13  45   5   2   0   0   1  0  0  0  0  48 191 0.71 0.21 0.10
#> 333  2.63  54   9   4   1   1   0  0  3  0  0  61 253 0.65 0.13 0.13
#> 334  0.00  23   3   1   0   0   3  0  0  0  0  25  84 0.67 0.15 0.05
#> 335  6.30 161  31  10   1   1   6  0  2  1  0 179 648 0.64 0.16 0.06
#> 336  7.71  71  14   7   1   0   4  0  0  0  0  77 281 0.67 0.13 0.08
#> 337  3.20  83  18   5   0   0   2  0  0  0  0  87 330 0.64 0.17 0.10
#> 338  4.95  79  11   9   0   0   2  0  1  1  0  89 350 0.63 0.17 0.12
#> 339  1.77  73  10   2   0   1   1  1  0  0  0  80 305 0.67 0.16 0.14
#> 340  1.10  58   7   1   0   0   0  0  0  0  0  65 280 0.66 0.18 0.16
#> 341  4.50  17   3   3   0   0   1  0  0  0  0  21  74 0.53 0.16 0.07
#> 342  4.76 180  34  13   0   4   6  1  4  2  1 201 703 0.63 0.18 0.06
#> 343  4.82  63   8   3   0   0   5  0  0  0  0  74 274 0.59 0.15 0.09
#> 344  2.73 100  23   5   1   0   4  2  4  2  1 112 417 0.66 0.17 0.08
#> 345  2.75 143  22   7   1   0   0  0  0  1  0 151 558 0.66 0.14 0.10
#> 346  3.77  53   9   3   1   0   3  0  0  1  1  58 230 0.64 0.15 0.13
#> 347  3.52  30   5   0   0   1   0  0  2  0  0  34 158 0.57 0.15 0.13
#> 348  3.45  60   7   2   1   1   0  1  2  0  0  66 279 0.65 0.15 0.12
#> 349  8.18  46  11   3   0   1   1  1  0  1  0  51 209 0.66 0.19 0.10
#> 350  0.63  52   7   2   1   1   1  0  1  0  0  58 201 0.73 0.13 0.21
#> 351  0.00  11   3   0   0   0   1  1  0  0  0  13  46 0.65 0.20 0.11
#> 352  2.45  23   0   1   0   0   0  1  1  0  0  24 114 0.71 0.18 0.14
#> 353  1.82  90  16   2   0   0   2  1  0  0  0 102 391 0.64 0.20 0.06
#> 354  3.25 169  36  10   0   1  10  1  1  1  0 181 659 0.65 0.18 0.05
#> 355  4.20  60  10   4   0   1   3  0  1  0  0  67 245 0.64 0.20 0.11
#> 356  3.48  37   4   2   0   0   0  0  1  0  0  43 194 0.64 0.19 0.08
#> 357  2.00 163  33   3   0   0   8  1  1  0  0 187 711 0.60 0.22 0.05
#> 358  1.69  63  13   0   0   0   0  0  1  0  0  67 245 0.71 0.16 0.10
#> 359  3.86  12   4   1   0   1   0  1  0  0  0  17  75 0.60 0.09 0.13
#> 360  3.00  79  11   3   0   0   0  1  0  0  0  82 275 0.71 0.20 0.09
#> 361  3.09  47  14   1   0   1   2  0  0  0  0  50 177 0.66 0.19 0.10
#> 362  4.75 170  31   9   2   0   2  0  4  0  0 181 682 0.67 0.23 0.04
#> 363  9.00   4   0   0   0   0   0  0  0  0  0   4  10 0.60 0.10 0.00
#> 364  2.45  27   4   1   0   0   0  1  0  0  0  28  98 0.69 0.10 0.13
#> 365  4.07 171  30   8   3   0   2  1  1  2  0 181 687 0.68 0.18 0.13
#> 366  7.27  38   9   3   0   0   2  0  1  1  0  41 139 0.67 0.18 0.06
#> 367  2.50  57   8   1   0   1   2  2  3  0  0  72 282 0.59 0.16 0.12
#> 368  6.61 189  39  11   0   0   4  2  7  3  0 207 759 0.64 0.17 0.09
#> 369  3.94  59  12   3   0   0   2  1  0  2  0  70 245 0.63 0.19 0.10
#> 370  1.76 186  27   8   1   1   6  0  0  0  0 200 722 0.69 0.18 0.13
#> 371  1.54  39   4   0   0   1   0  0  2  1  0  44 214 0.63 0.16 0.22
#> 372  4.22  43  13   3   0   2   0  1  0  1  0  50 217 0.59 0.15 0.09
#> 373  5.54  52   8   2   0   0   0  0  3  1  0  55 188 0.64 0.13 0.12
#> 374  3.30 175  36   6   3   1   5  3  4  2  2 197 739 0.66 0.17 0.11
#> 375  0.00   3   1   0   0   0   0  0  0  0  0   4  23 0.52 0.17 0.04
#> 376  2.64 154  25   3   0   0   3  1  2  0  0 177 637 0.65 0.15 0.14
#> 377  4.11  59  12   2   0   2   1  0  5  1  0  63 235 0.63 0.14 0.09
#> 378 15.00  15   4   2   0   0   0  0  0  0  0  20  84 0.51 0.17 0.06
#> 379  1.72  55   6   2   0   0   0  0  0  0  0  58 241 0.68 0.15 0.18
#> 380  3.09  46  12   2   0   0   1  0  2  0  0  50 195 0.63 0.17 0.07
#> 381  0.00  24   4   0   1   0   0  0  0  0  0  27 100 0.66 0.18 0.07
#> 382  6.43  32   9   0   0   0   0  0  2  0  0  33 130 0.64 0.17 0.11
#> 383  1.85  88  10   6   0   0   2  0  0  0  0  98 418 0.62 0.15 0.08
#> 384  6.75  16   1   2   0   0   0  0  0  0  0  17  61 0.64 0.18 0.07
#> 385  6.61  66  10   5   2   3   1  0  1  1  0  74 296 0.61 0.17 0.06
#> 386  4.15 181  32   5   0   0   3  2  1  1  0 204 740 0.64 0.18 0.12
#> 387  4.15  14   2   0   0   0   1  0  0  1  0  16  59 0.63 0.17 0.10
#> 388  2.57  25   2   0   0   0   0  0  1  0  0  29 116 0.58 0.18 0.08
#> 389  3.60 135  20   3   1   1   0  0  0  1  0 143 554 0.64 0.16 0.06
#> 390  3.86  28   5   3   0   0   0  0  1  1  0  30 117 0.65 0.16 0.08
#> 391  4.08  67   9   2   0   1   0  0  0  0  0  72 273 0.67 0.20 0.15
#> 392  0.00   4   1   0   0   0   0  0  0  0  0   4   9 0.78 0.00 0.11
#> 393  5.02  56   8   4   1   1   1  1  0  0  0  63 232 0.66 0.15 0.10
#> 394  6.89  63   8   6   1   1   1  1  0  0  0  74 282 0.59 0.17 0.09
#> 395  4.03 144  31   5   2   0   6  2  7  0  0 168 686 0.60 0.16 0.10
#> 396  3.13 114  16   4   0   0   3  0  1  0  0 127 502 0.63 0.16 0.08
#> 397  6.57  51  11   3   1   0   0  1  0  1  0  57 232 0.65 0.13 0.13
#> 398  0.64  44   5   1   0   1   3  1  2  0  0  50 210 0.68 0.20 0.19
#> 399  5.40  15   5   2   0   0   0  1  0  0  0  16  60 0.70 0.15 0.10
#> 400  3.23 181  30  12   0   0   3  2  3  0  0 200 827 0.63 0.21 0.07
#> 401  1.06  60   8   2   0   1   0  0  0  0  0  66 269 0.67 0.20 0.12
#> 402 10.67  67  18   6   0   0   2  2  1  0  0  79 282 0.63 0.15 0.09
#> 403  8.64  36  12   1   0   0   2  0  1  0  0  42 186 0.63 0.17 0.10
#> 404  5.11  49   8   4   1   1   1  0  2  1  1  53 210 0.65 0.19 0.17
#> 405  1.76  55  10   2   0   0   1  0  3  1  0  60 229 0.67 0.19 0.09
#> 406  0.00  55  10   0   0   1   2  0  0  0  0  60 259 0.69 0.16 0.09
#> 407 22.50  11   4   2   0   0   0  1  0  0  0  14  52 0.56 0.06 0.02
#> 408  2.66  78  15   4   0   0   1  0  0  0  0  80 301 0.69 0.15 0.14
#> 409  3.12  34   7   1   0   0   1  0  0  0  0  38 143 0.64 0.15 0.08
#> 410  3.31 183  43   5   2   2   7  1 11  4  1 210 812 0.63 0.19 0.15
#> 411  6.17  46   7   5   0   0   1  0  1  0  0  56 269 0.57 0.12 0.13
#> 412  5.14 118  22   7   2   1   1  0  2  0  0 130 455 0.65 0.17 0.10
#> 413  2.25  47  11   1   0   1   2  0  1  1  1  53 216 0.61 0.16 0.06
#> 414  3.26  70  10   1   1   0   3  0  0  0  0  77 292 0.65 0.20 0.08
#> 415  1.23  28   3   1   0   0   0  0  1  0  0  31 128 0.62 0.18 0.14
#> 416  5.20 183  37   6   2   0   3  3  1  1  0 194 710 0.66 0.20 0.10
#> 417  4.80  59  10   4   0   2   0  1  1  0  0  64 243 0.70 0.16 0.14
#> 418  3.42 178  29   6   2   1   4  2  1  1  0 196 796 0.66 0.16 0.12
#> 419  1.32 211  19   5   1   0   2  1  3  1  1 225 927 0.69 0.19 0.19
#> 420  4.88 218  47   8   4   0   6  3  3  0  0 235 843 0.70 0.16 0.11
#> 421  3.48 124  22   7   0   0   6  1  0  2  0 139 508 0.60 0.16 0.08
#> 422  3.94 176  24   6   2   0   5  0  6  1  0 189 733 0.62 0.18 0.10
#> 423  2.91 173  27   6   1   1   0  2  8  2  1 193 788 0.64 0.18 0.10
#> 424  6.10  41   5   2   0   0   1  0  0  0  0  44 164 0.63 0.16 0.17
#> 425  2.45  46   8   2   0   1   0  0  1  0  0  53 200 0.61 0.19 0.12
#> 426  3.86  59   8   0   0   2   3  0  0  0  0  66 271 0.63 0.15 0.08
#> 427  1.51 214  19  10   0   0   0  0  1  0  0 224 858 0.71 0.18 0.15
#> 428  0.00   7   1   1   0   0   0  0  0  0  0   7  24 0.67 0.21 0.04
#> 429  2.35  28   4   1   0   0   0  1  3  0  0  31 111 0.68 0.17 0.11
#> 430  4.26  76  15   3   0   0   1  0  2  2  0  78 294 0.74 0.26 0.11
#> 431  1.98  48   6   1   0   0   2  0  1  0  0  53 216 0.65 0.14 0.12
#> 432  3.33 191  31  12   1   0   8  0  4  1  0 212 775 0.63 0.15 0.14
#> 433  4.14 161  25   8   0   1   0  1  4  1  0 172 617 0.64 0.17 0.09
#> 434  1.76  53   6   2   0   0   1  0  0  0  0  57 253 0.60 0.13 0.15
#> 435  2.04  65   4   6   0   0   0  0  0  1  0  71 311 0.66 0.21 0.10
#> 436  3.16 160  20   9   4   1   2  2  1  3  0 181 686 0.62 0.14 0.10
#> 437  4.63  44   6   2   0   0   0  0  0  2  1  46 185 0.69 0.20 0.20
#> 438  1.80  52   8   1   0   0   1  0  1  1  0  55 209 0.65 0.23 0.12
#> 439  2.45  46   4   1   0   0   1  0  0  1  0  50 185 0.70 0.22 0.10
#> 440  0.59  56   8   2   0   0   1  0  3  1  1  61 218 0.68 0.20 0.16
#> 441  2.70  25   4   1   0   0   0  0  0  0  0  27 106 0.67 0.22 0.08
#> 442 11.25  16   3   1   0   1   0  1  0  0  0  19  74 0.58 0.12 0.07
#> 443  3.86  46   7   2   0   0   1  0  0  0  0  48 197 0.68 0.15 0.08
#> 444  3.38  22   5   2   0   0   1  0  0  0  0  23  90 0.63 0.16 0.09
#> 445  2.77  48   6   2   0   0   0  0  0  0  0  50 194 0.68 0.21 0.16
#> 446 11.08  61  11   5   1   0   1  1  1  0  0  67 252 0.66 0.17 0.07
#> 447  1.17  53   5   0   0   0   0  0  0  0  0  55 217 0.64 0.17 0.14
#> 448  2.51  51  10   1   0   0   0  0  2  0  0  54 225 0.66 0.20 0.14
#> 449  1.96  58   2   2   0   2   1  0  1  0  0  65 247 0.64 0.18 0.18
#> 450  8.76  43   4   2   1   1   2  1  0  1  1  50 197 0.64 0.17 0.10
#> 451  0.00   3   1   0   0   0   1  0  0  0  0   3   7 0.57 0.14 0.00
#> 452  4.03 177  37   4   4   1   3  2  3  0  0 193 776 0.64 0.16 0.10
#> 453  1.71  77  10   4   1   0   2  0  0  0  0  77 259 0.70 0.17 0.12
#> 454  3.94  61  12   1   1   1   2  0  3  0  0  63 236 0.68 0.16 0.14
#> 455  5.36 165  31   9   1   1   4  1  2  2  1 181 666 0.63 0.17 0.09
#> 456  2.84  46   4   3   0   0   1  0  0  0  0  49 167 0.72 0.17 0.09
#> 457  1.35  22   4   0   0   0   0  1  0  0  0  27 119 0.62 0.16 0.10
#> 458  4.80  58   8   6   0   2   1  1  0  0  0  67 251 0.63 0.15 0.09
#> 459  9.00  21   3   3   0   0   0  0  1  0  0  26  97 0.62 0.21 0.08
#> 460  8.59  31  11   1   0   0   0  1  0  0  0  39 135 0.60 0.13 0.10
#> 461  1.08  32   6   1   0   1   1  0  0  0  0  34 132 0.61 0.11 0.07
#> 462  5.03 148  25  11   0   0   4  2  1  0  0 170 652 0.63 0.17 0.08
#> 463  0.90  76  11   6   0   3   2  0  0  0  0  83 327 0.63 0.18 0.11
#> 464  3.86  28   8   0   0   0   0  1  2  0  0  33 114 0.65 0.19 0.06
#> 465  2.45  56  10   2   0   1   1  0  0  0  0  66 266 0.58 0.15 0.09
#> 466  5.09  65  15   2   2   3   2  0  0  0  0  72 275 0.64 0.13 0.09
#> 467  3.60  74  15   3   1   1   4  1  0  0  0  80 296 0.68 0.18 0.15
#> 468  9.00   4   0   1   0   0   0  0  0  0  0   6  25 0.52 0.20 0.08
#> 469  3.07  58   7   5   0   1   0  0  0  0  0  65 234 0.66 0.18 0.14
#> 470  3.68 107  22   5   0   0   7  1  0  0  0 120 460 0.60 0.15 0.08
#> 471  2.25  62  13   3   0   0   3  0  0  1  1  64 232 0.68 0.20 0.09
#> 472  1.29  22   0   2   0   0   1  0  2  0  0  24 107 0.65 0.17 0.19
#> 473 27.00   7   2   2   0   0   0  0  0  0  0   8  26 0.54 0.12 0.04
#> 474  4.66  36   5   3   1   0   0  0  1  1  0  42 191 0.64 0.17 0.10
#> 475  0.00  16   1   0   0   0   1  0  0  0  0  18  75 0.61 0.24 0.08
#> 476  4.24 126  23   5   2   1   3  1  1  1  1 137 542 0.63 0.17 0.09
#> 477  6.17  46   9   0   0   0   0  1  0  0  0  50 204 0.65 0.15 0.07
#> 478  3.38   9   2   0   0   1   0  1  0  0  0  11  39 0.64 0.13 0.15
#> 479  1.38  53  13   2   0   0   1  0  1  1  0  56 206 0.68 0.21 0.08
#> 480  1.42  23   3   2   0   0   1  0  0  1  0  23  89 0.71 0.22 0.08
#> 481  0.00  13   2   0   0   0   0  0  0  0  0  14  60 0.62 0.20 0.08
#> 482  3.02 176  26   9   2   0   5  2  3  0  0 198 773 0.62 0.19 0.06
#> 483  3.88 177  30   9   2   0   7  1  6  0  0 200 757 0.63 0.17 0.10
#> 484  2.82 167  25   6   1   1   2  1  2  0  0 181 713 0.68 0.15 0.11
#> 485  3.68 109  15   7   1   1   2  1  0  0  0 123 490 0.64 0.20 0.07
#> 486 12.27  18   6   1   1   0   0  1  1  0  0  21  79 0.65 0.16 0.05
#> 487  3.20 191  30   7   0   0   3  1  2  0  0 205 764 0.67 0.18 0.10
#> 488  2.93 164  24   6   0   0   4  1  2  2  1 180 684 0.63 0.18 0.08
#> 489  1.56  63  11   3   0   0   3  0  0  0  0  70 264 0.67 0.16 0.11
#> 490  4.56 205  31  13   1   0   3  2  6  1  1 220 772 0.66 0.18 0.08
#> 491  0.00  34   8   1   0   1   1  0  2  0  0  37 129 0.64 0.13 0.18
#> 492  2.63  52   6   3   1   1   2  0  1  0  0  59 206 0.65 0.18 0.13
#> 493  6.55  36   2   2   1   1   2  1  0  0  0  45 158 0.59 0.13 0.11
#> 494  0.00   6   1   0   0   0   1  0  0  0  0   7  34 0.62 0.15 0.06
#> 495 16.20   6   1   1   0   0   0  0  1  1  0   8  40 0.48 0.15 0.03
#> 496  4.02  62  14   4   1   0   4  0  0  0  0  72 279 0.67 0.14 0.14
#> 497  7.54 162  38   7   0   0   3  0  1  0  0 178 604 0.67 0.18 0.09
#> 498  1.40  91  15   2   1   1   1  2  0  1  0  98 339 0.70 0.18 0.08
#> 499  3.91 192  29  10   0   0   6  1  0  2  2 215 842 0.61 0.18 0.10
#> 500  9.00   4   1   0   0   0   1  0  0  0  0   4  16 0.56 0.19 0.06
#> 501  2.63  48  10   1   1   0   3  0  1  1  0  54 218 0.63 0.15 0.12
#> 502  2.12  63  11   7   0   0   3  1  1  0  0  68 231 0.64 0.16 0.06
#> 503  1.13  27   5   1   0   0   1  0  0  0  0  30  88 0.72 0.17 0.08
#> 504  2.74 175  37   6   0   1   4  1  3  0  3 193 697 0.67 0.19 0.09
#> 505  4.15  69  11   2   0   1   1  0  0  0  0  74 306 0.59 0.16 0.08
#> 506  1.93  55  14   1   0   0   2  0  1  1  0  59 226 0.64 0.17 0.06
#> 507  5.16 115  20   6   0   2   2  1  1  0  0 131 508 0.60 0.13 0.07
#> 508  4.23 148  22   7   0   0   0  1  2  2  0 160 597 0.66 0.19 0.10
#> 509  2.36 156  21   8   2   0   1  1  5  1  0 170 647 0.63 0.17 0.08
#> 510  1.62  59   7   3   0   1   3  0  0  0  0  65 250 0.64 0.16 0.10
#> 511  3.47 197  45  10   0   0   5  1  1  2  2 212 787 0.66 0.17 0.08
#>     GB.FB   LD   PU   WHIP BAbip  SO9  SO.W SO_perc uBB_perc SO_uBB
#> 1    0.27 0.18 0.18  1.364 0.182  4.9  0.67   0.118    0.118      0
#> 2    0.19 0.33 0.10  0.875 0.158  7.9  7.00   0.233    0.033      0
#> 3    0.32 0.26 0.11  1.200 0.333  8.1  3.00   0.231    0.077      0
#> 4    0.33 0.39 0.06  1.875 0.294  6.8  1.00   0.154    0.154      0
#> 5    0.50 0.35 0.04  1.773 0.348  6.8  1.83   0.164    0.075      0
#> 6    0.41 0.28 0.09  1.086 0.295 11.6  3.57   0.325    0.078      0
#> 7    0.44 0.19 0.14  0.915 0.306 13.3  4.14   0.403    0.097      0
#> 8    0.53 0.28 0.05  2.032 0.375  3.5  0.67   0.077    0.096      0
#> 9    0.50 0.25 0.06  1.125 0.306  9.5  3.50   0.259    0.074      0
#> 10   0.00 0.00 0.00  0.000 0.000  0.0    NA   0.000    0.000      0
#> 11   0.70 0.16 0.01  1.324 0.305  6.9  2.19   0.186    0.080      0
#> 12   0.48 0.31 0.04  1.186 0.296  4.6  2.75   0.124    0.045      0
#> 13   0.54 0.27 0.00  1.024 0.268  7.2  5.50   0.200    0.018      0
#> 14   0.59 0.27 0.05  1.816 0.415  8.5  2.00   0.197    0.082      0
#> 15   0.50 0.26 0.06  0.975 0.320 11.1  7.33   0.320    0.044      0
#> 16   0.49 0.31 0.04  1.156 0.315 10.2  4.25   0.285    0.067      0
#> 17   0.42 0.26 0.00  1.750 0.259  4.5  2.00   0.103    0.051      0
#> 18   0.54 0.38 0.00  3.000 0.273  6.8  0.43   0.130    0.304      0
#> 19   0.44 0.28 0.05  1.227 0.286  8.0  3.25   0.213    0.049      0
#> 20   0.60 0.14 0.00  0.830 0.171  7.5  2.60   0.217    0.083      0
#> 21   0.47 0.28 0.02  1.200 0.259  4.3  1.60   0.113    0.056      0
#> 22   0.75 0.00 0.00  0.857 0.500 15.4    NA   0.500    0.000      0
#> 23   0.38 0.23 0.06  1.683 0.356  7.2  2.75   0.172    0.062      0
#> 24   0.44 0.41 0.00  1.385 0.346  8.3  4.00   0.211    0.053      0
#> 25   0.53 0.32 0.06  1.537 0.326  4.0  1.20   0.102    0.085      0
#> 26   0.26 0.16 0.16  0.931 0.207  5.6  6.00   0.154    0.000      0
#> 27   0.36 0.19 0.12  1.099 0.235  9.1  2.29   0.254    0.111      0
#> 28   0.50 0.29 0.08  2.333 0.435  4.5  0.75   0.097    0.129      0
#> 29   0.51 0.32 0.03  2.091 0.432  7.4  1.29   0.164    0.091      0
#> 30   0.49 0.18 0.11  1.364 0.256  6.1  1.43   0.159    0.079      0
#> 31   0.82 0.18 0.00  0.500 0.091  4.5  2.00   0.143    0.071      0
#> 32   0.51 0.30 0.04  1.667 0.400  8.4  2.00   0.209    0.075      0
#> 33   0.47 0.33 0.00  1.650 0.286 10.8  1.33   0.276    0.207      0
#> 34   0.49 0.22 0.11  0.643 0.086  7.1  2.75   0.212    0.077      0
#> 35   0.38 0.33 0.13  1.412 0.346  7.4  3.50   0.194    0.056      0
#> 36   0.50 0.25 0.07  0.692 0.148 15.1  3.63   0.453    0.109      0
#> 37   0.31 0.34 0.09  1.800 0.367  6.5  3.00   0.150    0.050      0
#> 38   0.50 0.25 0.05  1.228 0.297  7.8  2.87   0.206    0.072      0
#> 39   0.43 0.24 0.07  1.545 0.333  4.1  2.50   0.100    0.040      0
#> 40   0.53 0.18 0.10  1.100 0.305  7.7  5.67   0.210    0.025      0
#> 41   0.58 0.20 0.05  0.831 0.217  7.5  3.60   0.212    0.047      0
#> 42   0.58 0.22 0.02  1.196 0.290  8.3  2.44   0.230    0.094      0
#> 43   0.33 0.33 0.03  1.568 0.351 11.0  2.57   0.277    0.092      0
#> 44   0.45 0.20 0.07  1.091 0.224  3.4  1.40   0.096    0.027      0
#> 45   0.35 0.16 0.12  0.764 0.200 11.3  4.60   0.333    0.072      0
#> 46   0.59 0.24 0.06  2.426 0.407  5.2  0.82   0.107    0.119      0
#> 47   0.31 0.25 0.06  1.821 0.400  5.8  1.50   0.140    0.070      0
#> 48   0.73 0.18 0.00  1.140 0.364 10.3  9.50   0.288    0.030      0
#> 49   0.33 0.33 0.17  1.875 0.500 10.1  1.50   0.273    0.182      0
#> 50   0.50 0.00 0.00  0.000 0.000  9.0    NA   0.333    0.000      0
#> 51   0.54 0.19 0.00  1.286 0.200  6.8  1.17   0.171    0.146      0
#> 52   0.47 0.37 0.07  1.600 0.372  8.4  2.33   0.212    0.076      0
#> 53   0.58 0.19 0.04  1.000 0.160  5.0  1.25   0.135    0.108      0
#> 54   0.52 0.22 0.04  1.116 0.296  7.2  3.67   0.200    0.055      0
#> 55   0.50 0.20 0.04  0.927 0.222  4.7  3.10   0.135    0.031      0
#> 56   0.42 0.32 0.08  1.047 0.280  8.3  5.11   0.235    0.041      0
#> 57   0.53 0.33 0.07  1.304 0.538 18.8  8.00   0.500    0.062      0
#> 58   0.50 0.30 0.02  1.233 0.340  8.3  3.85   0.226    0.059      0
#> 59   0.48 0.41 0.00  2.043 0.369  3.9  0.91   0.088    0.088      0
#> 60   1.00 0.00 0.00  1.000 0.000  9.0  1.00   0.333    0.333      0
#> 61   0.77 0.16 0.00  1.552 0.387  6.5  2.33   0.171    0.073      0
#> 62   0.39 0.19 0.17  0.735 0.135  9.4  2.43   0.279    0.115      0
#> 63   0.45 0.32 0.09  0.783 0.318 17.0  7.25   0.492    0.068      0
#> 64   0.52 0.29 0.03  1.524 0.369  9.0  3.50   0.216    0.062      0
#> 65   1.00 0.00 0.00  3.000 0.000  0.0  0.00   0.000    0.500      0
#> 66   0.44 0.29 0.09  1.500 0.294  5.1  1.20   0.122    0.102      0
#> 67   0.45 0.28 0.06  1.250 0.342  9.5  5.50   0.253    0.046      0
#> 68   0.50 0.35 0.08  2.250 0.360  2.7  0.40   0.059    0.118      0
#> 69   0.47 0.31 0.06  1.500 0.343  7.3  2.44   0.188    0.072      0
#> 70   0.34 0.29 0.14  1.275 0.281 11.5  2.83   0.298    0.105      0
#> 71   0.70 0.20 0.00  3.000 0.571  7.7  1.00   0.154    0.154      0
#> 72   0.47 0.24 0.09  1.231 0.313 11.1  4.00   0.291    0.073      0
#> 73   0.47 0.27 0.09  1.800 0.366  6.2  1.60   0.145    0.091      0
#> 74   0.61 0.26 0.02  1.140 0.286  4.9  3.00   0.130    0.043      0
#> 75   0.59 0.21 0.06  1.300 0.344  5.4  3.00   0.146    0.049      0
#> 76   0.24 0.32 0.16  1.528 0.389 15.8  2.58   0.383    0.148      0
#> 77   0.33 0.67 0.00  3.000 0.667 20.3  1.50   0.375    0.250      0
#> 78   0.42 0.32 0.09  1.137 0.307  7.5  4.09   0.212    0.052      0
#> 79   0.45 0.27 0.00  3.000 0.500  3.9    NA   0.077    0.000      0
#> 80   0.39 0.34 0.06  1.147 0.315  8.7  6.29   0.243    0.039      0
#> 81   0.74 0.17 0.00  0.750 0.182  3.4  3.00   0.100    0.033      0
#> 82   0.24 0.44 0.03  1.683 0.343 10.5  1.60   0.246    0.138      0
#> 83   0.56 0.29 0.00  2.206 0.450  6.4  1.33   0.143    0.071      0
#> 84   0.57 0.21 0.11  1.448 0.259  7.4  1.33   0.186    0.093      0
#> 85   0.20 0.40 0.11  1.404 0.295  9.2  2.00   0.229    0.114      0
#> 86   0.60 0.40 0.00  0.857 0.000  7.7  1.00   0.222    0.222      0
#> 87   0.37 0.21 0.16  0.682 0.222  9.8    NA   0.296    0.000      0
#> 88   0.48 0.27 0.04  1.030 0.302  9.4  4.83   0.266    0.055      0
#> 89   0.29 0.36 0.12  1.642 0.315  4.0  1.75   0.098    0.042      0
#> 90   0.42 0.31 0.05  1.769 0.352  5.8  1.56   0.141    0.079      0
#> 91   0.44 0.25 0.06  1.445 0.329  6.7  4.25   0.165    0.029      0
#> 92   1.00 0.00 0.00  0.000 0.000  0.0    NA   0.000    0.000      0
#> 93   0.33 0.00 0.17  0.000 0.000  0.0    NA   0.000    0.000      0
#> 94   0.54 0.34 0.05  1.273 0.341  4.1    NA   0.109    0.000      0
#> 95   0.50 0.35 0.00  1.313 0.303  6.8  4.00   0.174    0.043      0
#> 96   0.19 0.38 0.06  1.500 0.214  6.8  1.33   0.174    0.130      0
#> 97   0.42 0.33 0.09  1.235 0.381 10.1  6.33   0.275    0.043      0
#> 98   0.38 0.50 0.00  1.800 0.375 13.5  1.67   0.312    0.188      0
#> 99   0.56 0.13 0.13  0.857 0.267  7.7  3.00   0.261    0.087      0
#> 100  0.75 0.25 0.00  0.750 0.250  0.0    NA   0.000    0.000      0
#> 101  0.44 0.30 0.08  1.042 0.280  8.9  4.88   0.245    0.050      0
#> 102  0.39 0.31 0.11  1.467 0.316 10.2  2.13   0.262    0.123      0
#> 103  0.39 0.25 0.10  1.411 0.318  6.1  2.27   0.158    0.070      0
#> 104  0.47 0.16 0.11  1.000 0.231 10.6  2.50   0.299    0.104      0
#> 105  0.37 0.37 0.04  1.271 0.327  9.6  7.00   0.253    0.036      0
#> 106  0.54 0.25 0.03  1.402 0.268  7.5  1.55   0.194    0.126      0
#> 107  0.55 0.30 0.03  1.346 0.320  7.8  3.21   0.203    0.063      0
#> 108  0.40 0.10 0.00  1.333 0.222  6.0  2.00   0.143    0.071      0
#> 109  0.43 0.32 0.05  0.772 0.233  9.9  6.10   0.293    0.043      0
#> 110  0.41 0.28 0.14  0.732 0.111 10.5  3.20   0.320    0.100      0
#> 111  0.44 0.34 0.08  1.611 0.327  8.0  2.00   0.195    0.098      0
#> 112  0.42 0.30 0.09  1.457 0.318  5.6  1.61   0.143    0.079      0
#> 113  0.49 0.31 0.06  1.427 0.341  5.2  3.43   0.133    0.033      0
#> 114  0.31 0.41 0.03  1.680 0.379  4.3  2.00   0.105    0.053      0
#> 115  0.53 0.35 0.03  1.275 0.316  8.8  4.33   0.228    0.035      0
#> 116  0.41 0.25 0.09  1.380 0.272  6.8  2.11   0.174    0.082      0
#> 117  0.57 0.14 0.00  1.457 0.333 12.3  2.29   0.308    0.135      0
#> 118  0.00 0.50 0.00  1.000 0.500 18.0    NA   0.500    0.000      0
#> 119  0.45 0.20 0.10  1.435 0.211  8.2  1.17   0.212    0.182      0
#> 120  0.73 0.13 0.00  2.040 0.357  4.3  0.67   0.098    0.098      0
#> 121  0.27 0.23 0.13  2.192 0.250  7.3  0.70   0.149    0.213      0
#> 122  0.55 0.19 0.00  1.744 0.333  8.8  1.75   0.219    0.094      0
#> 123  0.35 0.32 0.08  1.184 0.286  9.2  4.33   0.245    0.057      0
#> 124  0.70 0.25 0.00  1.333 0.279  7.2  1.71   0.185    0.108      0
#> 125  0.12 0.47 0.12  2.118 0.313  7.9  0.83   0.179    0.214      0
#> 126  0.38 0.30 0.09  1.248 0.272  6.5  2.25   0.171    0.076      0
#> 127  0.33 0.17 0.17  3.000 0.400  0.0    NA   0.000    0.000      0
#> 128  0.52 0.28 0.04  1.583 0.358  6.2  2.64   0.151    0.057      0
#> 129  0.39 0.26 0.11  1.245 0.259  7.2  2.17   0.188    0.087      0
#> 130  0.33 0.33 0.00  3.000 0.600 16.2  3.00   0.300    0.100      0
#> 131  0.59 0.26 0.04  1.200 0.267  8.1  2.14   0.221    0.103      0
#> 132  0.55 0.15 0.00  2.000 0.368  1.8  1.00   0.042    0.042      0
#> 133  0.48 0.24 0.06  1.688 0.333  7.6  1.80   0.180    0.100      0
#> 134  0.48 0.35 0.04  1.286 0.308  6.7  2.38   0.179    0.075      0
#> 135  0.00 0.33 0.00  1.000 0.333  9.0    NA   0.250    0.000      0
#> 136  0.43 0.17 0.17  2.063 0.286  1.7  0.25   0.037    0.074      0
#> 137  0.38 0.17 0.08  0.837 0.208 13.8  3.14   0.415    0.132      0
#> 138  0.42 0.17 0.17  0.600 0.217  9.9 11.00   0.314    0.029      0
#> 139  0.40 0.28 0.09  1.444 0.346  8.1  2.86   0.205    0.067      0
#> 140  0.71 0.24 0.00  0.900 0.176  6.8  1.67   0.200    0.120      0
#> 141  0.50 0.29 0.03  2.455 0.452  4.9  4.00   0.103    0.026      0
#> 142  0.50 0.38 0.00  1.909 0.393  3.7  1.00   0.086    0.086      0
#> 143  0.46 0.23 0.15  1.364 0.308  2.5  1.00   0.062    0.062      0
#> 144  0.35 0.34 0.05  1.488 0.352  8.4  3.55   0.210    0.059      0
#> 145  0.25 0.75 0.00 12.000 1.000 27.0    NA   0.167    0.000      0
#> 146  0.17 0.17 0.00  2.000 0.000  4.5  0.33   0.091    0.273      0
#> 147  0.17 0.33 0.17  3.000 0.400  0.0    NA   0.000    0.000      0
#> 148  0.55 0.12 0.06  1.800 0.313  4.5  0.63   0.109    0.130      0
#> 149  0.53 0.24 0.03  1.161 0.250  4.4  1.67   0.116    0.070      0
#> 150  0.58 0.28 0.02  1.553 0.340  4.4  1.53   0.111    0.067      0
#> 151  0.41 0.46 0.00  1.364 0.410 11.0  9.00   0.286    0.016      0
#> 152  0.29 0.29 0.06  1.448 0.267  7.4  2.00   0.182    0.091      0
#> 153  0.00 0.33 0.17  1.200 0.200  5.4    NA   0.125    0.000      0
#> 154  0.52 0.23 0.10  0.353 0.033  4.8  6.00   0.154    0.026      0
#> 155  0.59 0.25 0.07  1.068 0.264  6.8  3.00   0.189    0.063      0
#> 156  0.38 0.28 0.08  1.630 0.304  4.7  1.00   0.119    0.119      0
#> 157  0.67 0.18 0.04  0.902 0.248  6.1  4.00   0.185    0.046      0
#> 158  0.60 0.00 0.00  2.143 0.250  7.7  0.50   0.200    0.400      0
#> 159  0.54 0.26 0.04  1.440 0.286  9.2  2.13   0.230    0.081      0
#> 160  0.28 0.35 0.11  1.440 0.341 11.9  4.40   0.297    0.068      0
#> 161  0.42 0.30 0.10  1.468 0.324  6.8  2.69   0.170    0.053      0
#> 162  0.39 0.11 0.39  1.000 0.222  1.8  1.00   0.048    0.048      0
#> 163  0.37 0.37 0.02  2.679 0.465  4.8  1.67   0.091    0.036      0
#> 164  0.34 0.14 0.11  0.750 0.147  8.0  2.60   0.241    0.074      0
#> 165  0.55 0.27 0.05  1.164 0.282  7.7  3.80   0.209    0.044      0
#> 166  0.41 0.29 0.10  1.149 0.308 10.9  3.80   0.297    0.062      0
#> 167  0.48 0.15 0.04  0.900 0.239  7.0  3.25   0.206    0.048      0
#> 168  0.43 0.29 0.02  1.063 0.262  9.0  3.20   0.242    0.045      0
#> 169  0.40 0.40 0.00  0.857 0.200  3.9  1.00   0.143    0.143      0
#> 170  0.41 0.24 0.07  1.378 0.342  7.3  5.00   0.192    0.038      0
#> 171  0.55 0.19 0.10  1.174 0.292  5.9  2.50   0.160    0.053      0
#> 172  0.53 0.22 0.07  1.000 0.183  3.0  0.83   0.085    0.103      0
#> 173  0.58 0.28 0.02  1.661 0.361  7.0  1.71   0.172    0.095      0
#> 174  0.39 0.29 0.10  1.035 0.218  7.2  3.00   0.197    0.059      0
#> 175  0.30 0.45 0.10  1.583 0.432  9.8 13.00   0.232    0.018      0
#> 176  0.33 0.26 0.05  2.613 0.410  5.2  0.67   0.103    0.138      0
#> 177  0.22 0.33 0.11  1.000 0.333  9.0    NA   0.250    0.000      0
#> 178  0.50 0.32 0.04  2.857 0.519  7.7  1.00   0.150    0.125      0
#> 179  0.35 0.34 0.06  1.421 0.345  6.6  7.00   0.175    0.025      0
#> 180  0.53 0.23 0.05  1.160 0.276  6.8  3.33   0.185    0.056      0
#> 181  0.60 0.15 0.07  0.911 0.241  7.9  4.60   0.225    0.049      0
#> 182  0.41 0.34 0.12  1.441 0.309  6.1  2.88   0.151    0.039      0
#> 183  0.56 0.22 0.08  1.462 0.382 12.5  4.50   0.310    0.069      0
#> 184  0.52 0.25 0.08  1.012 0.290  8.1  6.13   0.232    0.038      0
#> 185  0.20 0.33 0.16  1.163 0.298  9.4  4.25   0.246    0.058      0
#> 186  0.56 0.25 0.06  1.304 0.313 12.9  2.44   0.344    0.141      0
#> 187  0.60 0.00 0.20  0.000 0.000  7.7    NA   0.286    0.000      0
#> 188  0.50 0.30 0.00  2.000 0.500  6.0  2.00   0.154    0.000      0
#> 189  0.50 0.17 0.00  1.750 0.364 11.3  2.50   0.263    0.105      0
#> 190  0.37 0.30 0.09  1.457 0.308  4.9  2.11   0.123    0.052      0
#> 191  0.41 0.24 0.16  1.216 0.306  8.0  3.67   0.216    0.039      0
#> 192  0.54 0.28 0.04  1.051 0.238  5.7  2.20   0.153    0.069      0
#> 193  0.49 0.29 0.04  1.272 0.322  8.2  7.00   0.219    0.023      0
#> 194  0.51 0.32 0.05  0.974 0.286 10.2  5.70   0.284    0.050      0
#> 195  0.37 0.37 0.04  0.920 0.252 10.1  5.67   0.280    0.038      0
#> 196  0.45 0.33 0.03  1.615 0.368  6.2  2.14   0.153    0.061      0
#> 197  0.63 0.25 0.00  1.000 0.250  9.0  3.00   0.231    0.077      0
#> 198  0.38 0.26 0.09  1.500 0.351  6.4  2.50   0.163    0.059      0
#> 199  0.40 0.24 0.08  1.208 0.255  6.5  2.25   0.173    0.072      0
#> 200  0.29 0.32 0.07  0.857 0.250  9.4  4.25   0.279    0.049      0
#> 201  0.30 0.28 0.16  1.053 0.248  7.2  4.00   0.196    0.044      0
#> 202  0.49 0.17 0.03  2.276 0.382  3.7  0.57   0.083    0.146      0
#> 203  0.51 0.18 0.05  0.600 0.135  8.6  5.33   0.276    0.052      0
#> 204  0.45 0.24 0.09  1.043 0.276  9.6  5.56   0.270    0.043      0
#> 205  0.39 0.32 0.03  1.800 0.323  4.3  1.00   0.100    0.100      0
#> 206  0.57 0.29 0.00  1.200 0.308  9.0  5.00   0.250    0.050      0
#> 207  0.46 0.24 0.09  1.150 0.225  6.8  2.31   0.181    0.072      0
#> 208  0.60 0.07 0.20  0.600 0.133  0.0  0.00   0.000    0.062      0
#> 209  0.50 0.26 0.09  1.208 0.324  7.5  5.00   0.204    0.041      0
#> 210  0.48 0.27 0.02  1.018 0.286  9.6  6.67   0.267    0.027      0
#> 211  0.25 0.50 0.25  1.800 0.333  5.4  0.67   0.143    0.214      0
#> 212  0.57 0.29 0.05  1.163 0.230  7.9  2.15   0.216    0.101      0
#> 213  0.50 0.28 0.04  1.598 0.331  4.3  1.42   0.110    0.077      0
#> 214  0.61 0.18 0.08  0.951 0.243  7.9  4.00   0.226    0.057      0
#> 215  0.53 0.31 0.03  1.141 0.290  8.4  3.67   0.222    0.051      0
#> 216  0.49 0.32 0.05  1.500 0.343  6.8  2.25   0.176    0.078      0
#> 217  0.29 0.40 0.06  2.036 0.469  8.7  4.50   0.200    0.044      0
#> 218  0.48 0.26 0.11  1.588 0.269 10.3  1.30   0.255    0.196      0
#> 219  0.51 0.22 0.10  0.833 0.184  6.0  2.00   0.176    0.088      0
#> 220  0.55 0.16 0.00  0.686 0.152  6.2  2.67   0.178    0.044      0
#> 221  0.46 0.31 0.06  1.119 0.277 10.5  2.88   0.291    0.101      0
#> 222  0.60 0.24 0.05  1.380 0.305  4.7  2.00   0.123    0.062      0
#> 223  0.25 0.63 0.13  2.000 0.571  9.0    NA   0.222    0.000      0
#> 224  0.66 0.16 0.02  1.214 0.279  4.5  1.75   0.119    0.051      0
#> 225  0.38 0.29 0.09  1.136 0.270  4.4  4.60   0.122    0.026      0
#> 226  0.58 0.25 0.04  1.140 0.327  5.4 10.00   0.156    0.000      0
#> 227  0.39 0.30 0.13  1.185 0.313  8.3  4.75   0.228    0.048      0
#> 228  0.38 0.38 0.08  1.750 0.308  6.8  1.00   0.158    0.158      0
#> 229  0.32 0.32 0.08  1.475 0.356  9.6  3.00   0.233    0.078      0
#> 230  0.49 0.19 0.07  1.109 0.256  5.9  1.67   0.169    0.085      0
#> 231  0.44 0.06 0.44  0.333 0.167 16.5    NA   0.537    0.000      1
#> 232  0.30 0.38 0.08  1.182 0.270  3.3  1.33   0.089    0.044      0
#> 233  0.53 0.31 0.00  1.380 0.395 11.9  4.40   0.310    0.042      0
#> 234  0.52 0.23 0.10  1.889 0.407  8.0  2.00   0.182    0.068      0
#> 235  0.54 0.25 0.06  1.500 0.286  7.3  1.27   0.184    0.145      0
#> 236  0.38 0.39 0.09  1.448 0.370  9.6  3.64   0.243    0.067      0
#> 237  0.56 0.19 0.04  1.111 0.264  7.0  2.33   0.189    0.054      0
#> 238  0.50 0.00 0.00  1.500 0.000  0.0  0.00   0.000    0.250      0
#> 239  0.48 0.33 0.02  1.781 0.381  4.2  1.67   0.098    0.059      0
#> 240  0.58 0.24 0.05  1.222 0.309  7.0  3.50   0.184    0.053      0
#> 241  0.74 0.17 0.00  1.161 0.227  9.6  1.83   0.275    0.150      0
#> 242  0.45 0.32 0.06  1.397 0.331  7.9  2.83   0.202    0.071      0
#> 243  0.44 0.32 0.06  1.391 0.308  8.1  1.94   0.209    0.108      0
#> 244  0.58 0.23 0.09  1.333 0.310  9.6  2.67   0.246    0.092      0
#> 245  0.51 0.18 0.04  0.962 0.302  9.7  9.50   0.288    0.030      0
#> 246  0.53 0.30 0.03  1.514 0.320  5.8  1.71   0.145    0.085      0
#> 247  0.39 0.29 0.10  1.277 0.244  3.7  1.24   0.096    0.073      0
#> 248  0.39 0.33 0.06  1.362 0.322  9.1  3.38   0.237    0.070      0
#> 249  0.52 0.25 0.08  0.994 0.279 11.8  4.44   0.329    0.069      0
#> 250  0.65 0.26 0.03  1.096 0.256  7.4  3.07   0.206    0.067      0
#> 251  0.45 0.45 0.00  1.326 0.379 14.4  3.29   0.383    0.100      0
#> 252  0.63 0.26 0.00  2.167 0.474  9.0  1.50   0.207    0.138      0
#> 253  0.34 0.34 0.10  1.909 0.308  7.4  2.00   0.158    0.079      0
#> 254  0.45 0.24 0.06  0.966 0.307 10.8  6.45   0.314    0.049      0
#> 255  0.48 0.26 0.02  1.333 0.300  9.0  2.50   0.238    0.095      0
#> 256  0.50 0.24 0.08  1.261 0.267  6.7  1.79   0.179    0.100      0
#> 257  0.37 0.35 0.04  0.720 0.191  5.9 11.00   0.177    0.016      0
#> 258  0.54 0.23 0.08  3.000 0.545  9.8  1.33   0.182    0.136      0
#> 259  0.41 0.26 0.08  1.252 0.299  6.8  3.17   0.179    0.052      0
#> 260  0.33 0.33 0.00  1.000 0.333  9.0    NA   0.250    0.000      0
#> 261  0.43 0.36 0.06  1.464 0.384  9.6  5.00   0.240    0.048      0
#> 262  0.44 0.28 0.13  1.227 0.242 11.0  2.00   0.295    0.148      0
#> 263  0.54 0.31 0.01  1.543 0.318  5.5  1.75   0.137    0.073      0
#> 264  0.46 0.31 0.12  1.594 0.370 11.0  1.86   0.271    0.146      0
#> 265  0.50 0.25 0.00  3.000 0.500  0.0    NA   0.000    0.000      0
#> 266  0.67 0.13 0.07  1.313 0.400 11.8    NA   0.304    0.000      0
#> 267  0.44 0.44 0.13  2.500 0.438  6.8  1.00   0.136    0.091      0
#> 268  0.54 0.26 0.07  1.349 0.317  8.5  3.13   0.224    0.071      0
#> 269  0.39 0.29 0.10  1.272 0.315  7.0  4.88   0.178    0.032      0
#> 270  0.42 0.27 0.03  1.333 0.333 10.5  2.80   0.269    0.058      0
#> 271  0.37 0.26 0.07  1.500 0.266  7.0  1.38   0.175    0.127      0
#> 272  0.39 0.11 0.06  1.286 0.143 10.3  2.00   0.276    0.138      0
#> 273  0.64 0.19 0.04  1.027 0.292 11.1  5.08   0.308    0.061      0
#> 274  0.44 0.22 0.11  1.737 0.375  9.9  1.75   0.250    0.071      0
#> 275  0.54 0.24 0.03  1.773 0.333  3.1  1.00   0.075    0.075      0
#> 276  0.52 0.22 0.04  1.466 0.306  6.9  1.79   0.173    0.097      0
#> 277  0.31 0.25 0.16  1.629 0.290 10.8  1.75   0.246    0.140      0
#> 278  0.43 0.30 0.07  1.370 0.322  6.7  3.40   0.172    0.045      0
#> 279  0.72 0.16 0.04  0.618 0.080  7.1  1.80   0.231    0.128      0
#> 280  0.42 0.36 0.06  1.460 0.250  5.0  0.88   0.132    0.138      0
#> 281  0.58 0.30 0.00  1.024 0.308  8.6 13.00   0.232    0.018      0
#> 282  0.42 0.33 0.08  1.263 0.326 10.4  2.44   0.282    0.103      0
#> 283  0.49 0.40 0.07  1.459 0.372  6.6  4.50   0.161    0.036      0
#> 284  0.42 0.21 0.06  1.316 0.309  8.3  2.69   0.216    0.080      0
#> 285  0.42 0.28 0.05  1.446 0.327 10.1  4.20   0.250    0.060      0
#> 286  0.38 0.38 0.02  1.688 0.333  5.9  2.33   0.135    0.058      0
#> 287  0.61 0.14 0.00  1.263 0.278  8.5  2.40   0.218    0.091      0
#> 288  0.57 0.20 0.06  1.412 0.400  6.4  4.00   0.167    0.021      0
#> 289  0.00 0.00 1.00  1.000 0.000 18.0  2.00   0.500    0.250      0
#> 290  0.30 0.37 0.09  1.233 0.263  7.8  3.25   0.211    0.057      0
#> 291  0.29 0.14 0.00  4.000 0.571  0.0    NA   0.000    0.000      0
#> 292  0.48 0.32 0.00  2.027 0.391  6.6  2.25   0.143    0.048      0
#> 293  0.80 0.20 0.00  1.500 0.400  0.0    NA   0.000    0.000      0
#> 294  0.56 0.25 0.00  2.400 0.467  5.4    NA   0.111    0.000      0
#> 295  0.29 0.47 0.06  2.750 0.500  4.5  0.67   0.095    0.143      0
#> 296  0.61 0.19 0.05  1.103 0.277  9.9  2.94   0.273    0.093      0
#> 297  0.39 0.35 0.06  1.448 0.291  6.6  1.75   0.161    0.083      0
#> 298  0.54 0.24 0.07  1.174 0.238  7.0  2.40   0.190    0.048      0
#> 299  0.50 0.17 0.00  3.000 0.400  3.9  1.00   0.071    0.071      0
#> 300  0.68 0.23 0.02  1.200 0.298  5.4  3.00   0.148    0.049      0
#> 301  0.32 0.26 0.13  1.371 0.345 11.6  3.00   0.288    0.096      0
#> 302  0.50 0.19 0.17  0.579 0.167  9.0  6.33   0.268    0.042      0
#> 303  0.43 0.26 0.05  1.259 0.331  8.5  4.50   0.226    0.050      0
#> 304  0.34 0.34 0.05  1.935 0.359  4.4  0.83   0.100    0.100      0
#> 305  0.53 0.40 0.03  3.000 0.571  9.5  2.33   0.179    0.077      0
#> 306  0.48 0.24 0.10  1.118 0.275 11.6  3.67   0.310    0.070      0
#> 307  0.52 0.22 0.05  1.017 0.267 10.3  3.54   0.280    0.073      0
#> 308  0.75 0.08 0.00  2.842 0.455 11.4  1.14   0.211    0.158      0
#> 309  0.22 0.37 0.07  0.769 0.308 14.5 21.00   0.429    0.020      0
#> 310  0.42 0.35 0.00  2.036 0.360 11.6  3.00   0.255    0.085      0
#> 311  0.41 0.28 0.05  1.543 0.350  7.1  2.64   0.175    0.066      0
#> 312  0.83 0.00 0.00  0.000 0.000  0.0    NA   0.000    0.000      0
#> 313  0.66 0.19 0.03  0.836 0.219  4.9  5.50   0.141    0.013      0
#> 314  0.50 0.20 0.07  1.292 0.295  6.0  2.64   0.158    0.060      0
#> 315  0.62 0.19 0.05  0.656 0.200 14.3  8.50   0.405    0.048      0
#> 316  0.60 0.20 0.00  1.200 0.400 10.8    NA   0.286    0.000      0
#> 317  0.51 0.23 0.08  1.108 0.266  5.8  2.27   0.160    0.061      0
#> 318  0.46 0.20 0.11  1.000 0.250  6.2  6.50   0.176    0.027      0
#> 319  0.18 0.45 0.09  1.333 0.364  6.0    NA   0.154    0.000      0
#> 320  0.43 0.29 0.06  1.139 0.247  4.1  1.50   0.113    0.075      0
#> 321  0.65 0.19 0.00  0.867 0.190  5.4  2.25   0.158    0.070      0
#> 322  0.30 0.50 0.10  3.000 0.545 12.0  1.33   0.222    0.167      0
#> 323  0.50 0.00 0.00  9.000 0.500  0.0  0.00   0.000    0.375      0
#> 324  0.61 0.29 0.05  1.714 0.410  7.7  3.33   0.185    0.056      0
#> 325  0.69 0.18 0.04  0.990 0.229  4.1  1.88   0.116    0.062      0
#> 326  0.45 0.25 0.15  1.371 0.243  4.6  1.20   0.120    0.100      0
#> 327  0.26 0.34 0.03  1.286 0.286  6.9  2.25   0.180    0.020      0
#> 328  0.43 0.50 0.00  1.059 0.286  9.5  6.00   0.273    0.045      0
#> 329  0.25 0.00 0.25  9.000 0.333  0.0  0.00   0.000    0.167      0
#> 330  0.50 0.32 0.04  1.585 0.331  6.7  1.75   0.163    0.084      0
#> 331  0.41 0.35 0.08  2.129 0.351  3.5  0.57   0.074    0.093      0
#> 332  0.38 0.24 0.06  0.868 0.212  7.8  3.67   0.229    0.062      0
#> 333  0.50 0.21 0.02  1.463 0.341  8.6  2.17   0.213    0.082      0
#> 334  0.48 0.33 0.00  0.857 0.190  2.6  1.00   0.080    0.080      0
#> 335  0.53 0.23 0.05  1.550 0.339  6.8  2.31   0.168    0.067      0
#> 336  0.43 0.36 0.13  1.898 0.355  3.3  1.00   0.078    0.078      0
#> 337  0.47 0.24 0.05  1.424 0.377  9.6  5.25   0.241    0.046      0
#> 338  0.46 0.30 0.05  1.500 0.333  7.2  2.29   0.180    0.079      0
#> 339  0.42 0.19 0.08  0.984 0.261 11.5  4.33   0.325    0.062      0
#> 340  0.26 0.38 0.15  0.857 0.242 13.2  4.80   0.369    0.077      0
#> 341  0.64 0.21 0.00  2.500 0.400  4.5  0.50   0.095    0.190      0
#> 342  0.44 0.25 0.06  1.500 0.305  4.0  1.43   0.100    0.050      0
#> 343  0.53 0.29 0.02  1.179 0.224  4.8  1.43   0.135    0.095      0
#> 344  0.48 0.27 0.07  1.443 0.345  6.2  2.00   0.161    0.080      0
#> 345  0.42 0.34 0.05  1.167 0.280  7.8  4.43   0.205    0.046      0
#> 346  0.44 0.29 0.09  1.395 0.310  5.7  1.80   0.155    0.086      0
#> 347  0.33 0.33 0.05  1.435 0.263 10.6  2.25   0.265    0.088      0
#> 348  0.26 0.21 0.17  0.957 0.244 10.9  4.75   0.288    0.045      0
#> 349  0.54 0.33 0.05  1.727 0.378  6.5  2.67   0.157    0.039      0
#> 350  0.56 0.26 0.06  0.977 0.286 10.7  4.25   0.293    0.052      0
#> 351  0.40 0.30 0.00  1.200 0.300  5.4  2.00   0.154    0.077      0
#> 352  0.50 0.28 0.06  0.273 0.059  7.4    NA   0.250    0.000      0
#> 353  0.48 0.14 0.06  1.257 0.243  5.5  1.36   0.147    0.108      0
#> 354  0.51 0.23 0.05  1.331 0.315  4.3  2.10   0.116    0.050      0
#> 355  0.63 0.29 0.02  1.467 0.298  6.6  1.83   0.164    0.075      0
#> 356  0.42 0.27 0.00  0.871 0.231  9.6  3.67   0.256    0.070      0
#> 357  0.52 0.26 0.03  1.289 0.288  7.2  1.89   0.193    0.102      0
#> 358  0.33 0.33 0.10  1.188 0.283  8.4  3.75   0.224    0.060      0
#> 359  0.25 0.50 0.00  3.857 0.625 19.3  1.25   0.294    0.176      0
#> 360  0.33 0.27 0.16  0.857 0.219  5.6 13.00   0.159    0.012      0
#> 361  0.63 0.24 0.00  1.457 0.417  8.5  5.50   0.220    0.020      0
#> 362  0.49 0.30 0.03  1.320 0.313  6.7  3.88   0.171    0.044      0
#> 363  0.00 0.25 0.00  1.000 0.000  0.0    NA   0.000    0.000      0
#> 364  0.13 0.26 0.17  0.818 0.227  6.1    NA   0.179    0.000      0
#> 365  0.50 0.26 0.07  1.310 0.350 10.5  6.13   0.271    0.044      0
#> 366  0.42 0.24 0.06  1.962 0.387  5.2  1.67   0.122    0.073      0
#> 367  0.54 0.26 0.10  1.056 0.188  5.5  1.10   0.153    0.125      0
#> 368  0.43 0.26 0.06  1.427 0.329  6.0  2.91   0.155    0.053      0
#> 369  0.53 0.29 0.04  1.563 0.306  6.2  1.10   0.157    0.143      0
#> 370  0.35 0.32 0.07  1.000 0.283  9.7  5.00   0.275    0.050      0
#> 371  0.47 0.24 0.06  0.857 0.235 16.2  4.20   0.477    0.091      0
#> 372  0.44 0.28 0.03  2.063 0.432  5.9  1.17   0.140    0.080      0
#> 373  0.69 0.17 0.05  1.231 0.244  5.5  2.67   0.145    0.055      0
#> 374  0.51 0.26 0.04  1.360 0.331  7.6  2.60   0.198    0.071      0
#> 375  0.33 0.33 0.00  3.000 0.333  0.0  0.00   0.000    0.250      0
#> 376  0.43 0.28 0.06  1.015 0.243  7.5  2.64   0.209    0.079      0
#> 377  0.61 0.16 0.00  1.239 0.298  6.5  2.75   0.175    0.032      0
#> 378  0.46 0.15 0.15  3.333 0.462  6.0  0.50   0.100    0.200      0
#> 379  0.43 0.17 0.14  0.766 0.242 12.1  7.00   0.362    0.052      0
#> 380  0.33 0.41 0.05  1.457 0.378  6.2  4.00   0.160    0.040      0
#> 381  0.25 0.45 0.05  1.050 0.250  5.4  2.00   0.148    0.074      0
#> 382  0.48 0.35 0.04  1.714 0.429 11.6  9.00   0.273    0.030      0
#> 383  0.35 0.35 0.04  1.110 0.235  6.7  2.00   0.184    0.092      0
#> 384  0.23 0.08 0.08  1.250 0.250  6.8  3.00   0.176    0.059      0
#> 385  0.48 0.30 0.09  1.653 0.327  6.6  1.50   0.162    0.068      0
#> 386  0.55 0.22 0.08  1.259 0.268  7.6  2.22   0.196    0.088      0
#> 387  0.54 0.31 0.00  1.154 0.167  2.1  0.50   0.062    0.125      0
#> 388  0.53 0.24 0.06  1.143 0.133 10.3  2.00   0.276    0.138      0
#> 389  0.48 0.30 0.06  1.114 0.226  5.4  3.00   0.147    0.042      0
#> 390  0.48 0.24 0.08  1.429 0.320  3.9  1.50   0.100    0.067      0
#> 391  0.30 0.34 0.17  1.019 0.250 10.2  5.00   0.278    0.042      0
#> 392  0.67 0.00 0.00  1.000 0.333  9.0    NA   0.250    0.000      0
#> 393  0.33 0.29 0.15  1.395 0.283  6.3  1.67   0.159    0.079      0
#> 394  0.42 0.23 0.09  1.596 0.294  5.7  1.43   0.135    0.081      0
#> 395  0.50 0.32 0.02  1.553 0.349  8.5  1.80   0.214    0.119      0
#> 396  0.48 0.24 0.06  1.042 0.235  7.7  2.45   0.213    0.087      0
#> 397  0.46 0.21 0.03  1.541 0.375  8.8  3.00   0.211    0.070      0
#> 398  0.57 0.17 0.10  0.786 0.200  9.6  3.00   0.300    0.080      0
#> 399  0.33 0.53 0.00  2.100 0.500  5.4    NA   0.125    0.000      0
#> 400  0.48 0.24 0.05  1.246 0.288  6.5  2.43   0.170    0.070      0
#> 401  0.51 0.18 0.05  0.941 0.250 10.1  3.80   0.288    0.061      0
#> 402  0.54 0.30 0.05  2.442 0.453  8.8  1.56   0.177    0.114      0
#> 403  0.52 0.28 0.07  2.280 0.448  7.6  1.17   0.167    0.143      0
#> 404  0.55 0.30 0.03  1.378 0.406 11.7  5.33   0.302    0.038      0
#> 405  0.45 0.34 0.09  1.043 0.279  6.5  3.67   0.183    0.050      0
#> 406  0.45 0.13 0.25  0.957 0.250  8.6  3.00   0.250    0.067      0
#> 407  0.50 0.33 0.00  4.000 0.545  0.0  0.00   0.000    0.071      0
#> 408  0.57 0.23 0.04  1.033 0.345 10.2 11.50   0.288    0.025      0
#> 409  0.61 0.25 0.07  1.385 0.320  8.3  2.67   0.211    0.079      0
#> 410  0.64 0.22 0.01  1.429 0.373  9.2  2.50   0.238    0.086      0
#> 411  0.33 0.36 0.03  1.971 0.400 10.0  1.63   0.232    0.143      0
#> 412  0.52 0.34 0.01  1.643 0.330  6.4  1.82   0.154    0.077      0
#> 413  0.50 0.16 0.11  1.583 0.300  4.5  1.00   0.113    0.094      0
#> 414  0.42 0.33 0.02  1.138 0.214  4.7  1.67   0.130    0.078      0
#> 415  0.79 0.11 0.00  0.955 0.222 11.0  4.50   0.290    0.065      0
#> 416  0.41 0.30 0.03  1.356 0.341  9.0  6.43   0.232    0.036      0
#> 417  0.28 0.30 0.13  1.267 0.333 10.2  4.25   0.266    0.031      0
#> 418  0.46 0.20 0.10  1.225 0.311 10.3  3.86   0.276    0.066      0
#> 419  0.40 0.22 0.10  0.636 0.217 13.6  9.30   0.413    0.044      0
#> 420  0.42 0.33 0.06  1.337 0.353  7.8  5.33   0.204    0.038      0
#> 421  0.58 0.24 0.03  1.337 0.282  4.8  1.50   0.129    0.086      0
#> 422  0.45 0.25 0.04  1.083 0.260  8.6  3.54   0.243    0.069      0
#> 423  0.25 0.26 0.13  1.165 0.283  9.1  3.92   0.244    0.057      0
#> 424  0.37 0.47 0.03  1.258 0.269 10.5  4.00   0.273    0.068      0
#> 425  0.64 0.22 0.00  1.455 0.286  8.2  2.00   0.189    0.075      0
#> 426  0.55 0.17 0.02  1.163 0.190  7.2  1.86   0.197    0.076      0
#> 427  0.32 0.29 0.11  0.704 0.213 11.2  8.22   0.330    0.040      0
#> 428  0.71 0.29 0.00  1.200 0.286  0.0    NA   0.000    0.000      0
#> 429  0.28 0.44 0.04  0.783 0.200  4.7  4.00   0.129    0.032      0
#> 430  0.46 0.30 0.07  1.211 0.340  9.0 19.00   0.244    0.013      0
#> 431  0.41 0.19 0.05  0.951 0.200  7.2  2.75   0.208    0.075      0
#> 432  0.46 0.28 0.04  1.286 0.333  9.5  3.18   0.255    0.080      0
#> 433  0.45 0.21 0.08  1.137 0.270  7.6  3.89   0.203    0.047      0
#> 434  0.36 0.18 0.08  0.848 0.222  9.4  4.00   0.281    0.070      0
#> 435  0.22 0.30 0.08  1.019 0.286 13.8  5.40   0.380    0.070      0
#> 436  0.38 0.29 0.10  1.266 0.268  7.4  2.06   0.193    0.088      0
#> 437  0.38 0.38 0.00  1.114 0.333 13.1  8.50   0.370    0.043      0
#> 438  0.56 0.26 0.03  0.733 0.281 11.4 19.00   0.345    0.018      0
#> 439  0.59 0.28 0.00  0.545 0.156  8.6  4.67   0.280    0.060      0
#> 440  0.51 0.23 0.03  0.913 0.270 11.2  4.75   0.311    0.066      0
#> 441  0.42 0.32 0.11  1.050 0.263  8.1  3.00   0.222    0.074      0
#> 442  0.27 0.33 0.13  2.000 0.308  4.5  1.00   0.105    0.053      0
#> 443  0.42 0.24 0.05  1.286 0.273  6.9  4.50   0.188    0.042      0
#> 444  0.62 0.19 0.00  1.688 0.412  6.8  4.00   0.174    0.043      0
#> 445  0.38 0.35 0.12  0.769 0.320 15.2 22.00   0.440    0.020      0
#> 446  0.46 0.37 0.04  2.000 0.386  9.0  3.25   0.194    0.060      0
#> 447  0.30 0.30 0.05  0.587 0.143  9.4  8.00   0.291    0.036      0
#> 448  0.39 0.33 0.12  0.977 0.333 11.3  6.00   0.333    0.056      0
#> 449  0.49 0.13 0.10  0.655 0.108  9.3  3.17   0.292    0.062      0
#> 450  0.31 0.34 0.06  1.297 0.219  6.6  1.50   0.180    0.100      0
#> 451  0.67 0.33 0.00  1.000 0.333  0.0    NA   0.000    0.000      0
#> 452  0.45 0.27 0.05  1.321 0.354  9.7  4.80   0.249    0.047      0
#> 453  0.58 0.21 0.04  0.810 0.278  9.0    NA   0.273    0.000      0
#> 454  0.52 0.18 0.05  1.000 0.318  9.6  8.50   0.270    0.016      0
#> 455  0.45 0.29 0.07  1.374 0.315  6.2  2.31   0.166    0.066      0
#> 456  0.46 0.24 0.08  0.789 0.200  6.4  9.00   0.184    0.020      0
#> 457  0.40 0.33 0.00  1.200 0.286 12.2  2.25   0.333    0.148      0
#> 458  0.30 0.32 0.06  1.533 0.318  7.8  1.86   0.194    0.075      0
#> 459  0.22 0.50 0.11  2.200 0.353  5.4  0.75   0.115    0.154      0
#> 460  0.38 0.45 0.03  2.318 0.400  2.5  0.40   0.051    0.128      0
#> 461  0.26 0.33 0.15  1.080 0.259  5.4  2.50   0.147    0.029      0
#> 462  0.41 0.29 0.09  1.449 0.298  5.9  1.44   0.153    0.106      0
#> 463  0.43 0.28 0.12  1.100 0.288  7.2  4.00   0.193    0.012      0
#> 464  0.70 0.13 0.04  1.714 0.381  9.0  2.33   0.212    0.091      0
#> 465  0.52 0.30 0.02  1.568 0.316  9.8  1.78   0.242    0.121      0
#> 466  0.53 0.31 0.03  1.472 0.352  5.1  1.67   0.139    0.042      0
#> 467  0.59 0.35 0.02  1.300 0.396 11.3  5.00   0.312    0.050      0
#> 468  0.33 0.67 0.00  3.000 0.333  9.0  0.50   0.167    0.333      0
#> 469  0.36 0.24 0.10  1.295 0.279  8.6  2.33   0.215    0.077      0
#> 470  0.55 0.27 0.02  1.330 0.290  4.0  1.30   0.108    0.083      0
#> 471  0.40 0.31 0.04  1.125 0.320  6.2 11.00   0.172    0.016      0
#> 472  0.29 0.07 0.14  0.571 0.143 10.3  4.00   0.333    0.083      0
#> 473  0.33 0.50 0.00  5.000 0.800  9.0    NA   0.125    0.000      0
#> 474  0.25 0.25 0.08  1.655 0.391 11.2  2.00   0.286    0.143      0
#> 475  0.64 0.09 0.00  0.529 0.083  6.4  2.00   0.222    0.111      0
#> 476  0.54 0.29 0.06  1.206 0.319  7.9  3.75   0.219    0.051      0
#> 477  0.40 0.17 0.05  1.371 0.231  3.1  1.33   0.080    0.060      0
#> 478  0.80 0.00 0.00  1.125 0.333 13.5  4.00   0.364    0.000      0
#> 479  0.47 0.26 0.05  1.462 0.357  6.9  3.33   0.179    0.054      0
#> 480  0.58 0.37 0.00  0.947 0.294  7.1    NA   0.217    0.000      0
#> 481  0.33 0.33 0.00  0.818 0.333 17.2  7.00   0.500    0.071      0
#> 482  0.46 0.23 0.09  1.196 0.261  6.4  1.89   0.172    0.091      0
#> 483  0.47 0.26 0.02  1.338 0.289  6.2  1.88   0.160    0.085      0
#> 484  0.38 0.30 0.05  1.052 0.264  8.7  3.91   0.238    0.055      0
#> 485  0.46 0.28 0.07  1.295 0.303  9.2  2.73   0.244    0.081      0
#> 486  0.53 0.24 0.00  2.727 0.500  4.9  2.00   0.095    0.048      0
#> 487  0.42 0.25 0.10  1.105 0.274  8.9  4.17   0.244    0.059      0
#> 488  0.43 0.22 0.08  1.043 0.240  6.7  2.83   0.189    0.067      0
#> 489  0.49 0.29 0.04  1.154 0.280  6.8  2.17   0.186    0.086      0
#> 490  0.35 0.27 0.16  1.200 0.263  4.7  2.55   0.127    0.050      0
#> 491  0.52 0.40 0.00  1.269 0.375 10.4  5.00   0.270    0.027      0
#> 492  0.53 0.35 0.00  1.171 0.263  8.6  2.60   0.220    0.068      0
#> 493  0.55 0.19 0.10  1.364 0.172  4.9  0.75   0.133    0.156      0
#> 494  0.33 0.33 0.00  1.000 0.167  0.0  0.00   0.000    0.143      0
#> 495  0.17 0.33 0.00  2.400 0.333  0.0  0.00   0.000    0.250      0
#> 496  0.40 0.29 0.07  1.660 0.422  9.8  2.43   0.236    0.097      0
#> 497  0.46 0.33 0.05  1.784 0.341  5.1  1.75   0.118    0.067      0
#> 498  0.49 0.14 0.06  0.857 0.231  5.3  3.75   0.153    0.031      0
#> 499  0.46 0.28 0.06  1.170 0.293  9.2  3.18   0.251    0.079      0
#> 500  0.50 0.25 0.00  2.000 0.333  0.0    NA   0.000    0.000      0
#> 501  0.54 0.32 0.03  1.317 0.316  6.6  1.67   0.185    0.111      0
#> 502  0.54 0.20 0.05  1.235 0.300  2.1  1.33   0.059    0.044      0
#> 503  0.36 0.32 0.04  0.750 0.240  2.3    NA   0.067    0.000      0
#> 504  0.44 0.26 0.04  1.283 0.316  7.0  3.00   0.187    0.057      0
#> 505  0.29 0.29 0.06  1.269 0.277  9.3  3.60   0.243    0.054      0
#> 506  0.48 0.29 0.04  1.214 0.319  4.5  7.00   0.119    0.017      0
#> 507  0.43 0.20 0.10  1.382 0.280  5.8  1.73   0.145    0.069      0
#> 508  0.46 0.21 0.09  1.174 0.242  5.4  2.30   0.144    0.062      0
#> 509  0.27 0.27 0.13  1.071 0.237  4.9  2.09   0.135    0.065      0
#> 510  0.72 0.18 0.02  1.020 0.208  4.9  1.80   0.138    0.062      0
#> 511  0.43 0.29 0.08  1.439 0.350  6.9  2.92   0.179    0.061      0
# }
```
