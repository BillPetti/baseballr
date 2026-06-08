# **Scrape Batter Performance Data Over a Custom Time Frame**

This function allows you to scrape basic batter statistics over a custom
time frame. Data is sourced from Baseball-Reference.com.

## Usage

``` r
bref_daily_batter(t1, t2)
```

## Arguments

- t1:

  First date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

- t2:

  Last date data should be scraped from. Should take the form
  "YEAR-MONTH-DAY"

## Value

Returns a tibble of batter performance over the requested date range,
one row per player, with the following columns:

|          |           |                                     |
|----------|-----------|-------------------------------------|
| col_name | types     | description                         |
| season   | integer   | Season year.                        |
| Name     | character | Player name.                        |
| Age      | numeric   | Player age during the season.       |
| Level    | character | League level (e.g. Maj-AL, Maj-NL). |
| Team     | character | Team name.                          |
| G        | numeric   | Games played.                       |
| PA       | numeric   | Plate appearances.                  |
| AB       | numeric   | At-bats.                            |
| R        | numeric   | Runs scored.                        |
| H        | numeric   | Hits.                               |
| X1B      | numeric   | Singles.                            |
| X2B      | numeric   | Doubles.                            |
| X3B      | numeric   | Triples.                            |
| HR       | numeric   | Home runs.                          |
| RBI      | numeric   | Runs batted in.                     |
| BB       | numeric   | Walks (bases on balls).             |
| IBB      | numeric   | Intentional walks.                  |
| uBB      | numeric   | Unintentional walks.                |
| SO       | numeric   | Strikeouts.                         |
| HBP      | numeric   | Times hit by pitch.                 |
| SH       | numeric   | Sacrifice hits (bunts).             |
| SF       | numeric   | Sacrifice flies.                    |
| GDP      | numeric   | Grounded into double plays.         |
| SB       | numeric   | Stolen bases.                       |
| CS       | numeric   | Times caught stealing.              |
| BA       | numeric   | Batting average.                    |
| OBP      | numeric   | On-base percentage.                 |
| SLG      | numeric   | Slugging percentage.                |
| OPS      | numeric   | On-base plus slugging.              |

## Examples

``` r
# \donttest{
  try(bref_daily_batter(t1="2015-05-10", t2="2015-06-20"))
#> ✖ 2026-06-08 01:55:11.37814: Invalid arguments or no daily batter data available!
#>     season                  Name Age         Level
#> 1     2015            José Abreu  28        Maj-AL
#> 2     2015         Dustin Ackley  27        Maj-AL
#> 3     2015            Matt Adams  26        Maj-NL
#> 4     2015        Jeremy Affeldt  36        Maj-NL
#> 5     2015            Nick Ahmed  25        Maj-NL
#> 6     2015        Hanser Alberto  22        Maj-AL
#> 7     2015       Abraham Almonte  26        Maj-NL
#> 8     2015         Yonder Alonso  28        Maj-NL
#> 9     2015           Jose Altuve  25        Maj-AL
#> 10    2015 Henderson Alvarez III  25        Maj-NL
#> 11    2015         Pedro Álvarez  28        Maj-NL
#> 12    2015        Alexi Amarista  26        Maj-NL
#> 13    2015        Brett Anderson  27        Maj-NL
#> 14    2015        Chase Anderson  27        Maj-NL
#> 15    2015         Matt Andriese  25        Maj-AL
#> 16    2015          Elvis Andrus  26        Maj-AL
#> 17    2015             Nori Aoki  33        Maj-NL
#> 18    2015          Chris Archer  26        Maj-AL
#> 19    2015         Nolan Arenado  24        Maj-NL
#> 20    2015         Joaquín Arias  30        Maj-NL
#> 21    2015          Jake Arrieta  29        Maj-NL
#> 22    2015            Cody Asche  25        Maj-NL
#> 23    2015      Phillippe Aumont  26        Maj-NL
#> 24    2015           Mike Aviles  34        Maj-AL
#> 25    2015           Erick Aybar  31        Maj-AL
#> 26    2015            Jeff Baker  34        Maj-NL
#> 27    2015          Clint Barmes  36        Maj-NL
#> 28    2015         Austin Barnes  25        Maj-NL
#> 29    2015        Brandon Barnes  29        Maj-NL
#> 30    2015       Tucker Barnhart  24        Maj-NL
#> 31    2015          Trevor Bauer  24        Maj-AL
#> 32    2015         José Bautista  34        Maj-AL
#> 33    2015           Mike Baxter  30        Maj-NL
#> 34    2015        Gordon Beckham  28        Maj-AL
#> 35    2015           Tim Beckham  25        Maj-AL
#> 36    2015      Ronald Belisario  32        Maj-AL
#> 37    2015          Brandon Belt  27        Maj-NL
#> 38    2015        Carlos Beltrán  38        Maj-AL
#> 39    2015         Adrian Beltré  36        Maj-AL
#> 40    2015          Doug Bernier  35        Maj-AL
#> 41    2015 Christian Bethancourt  23        Maj-NL
#> 42    2015           Chad Bettis  26        Maj-NL
#> 43    2015          Mookie Betts  22        Maj-AL
#> 44    2015          Jeff Bianchi  28        Maj-AL
#> 45    2015      Chad Billingsley  30        Maj-NL
#> 46    2015      Charlie Blackmon  28        Maj-NL
#> 47    2015         Andrés Blanco  31        Maj-NL
#> 48    2015         Grégor Blanco  31        Maj-NL
#> 49    2015           Kyle Blanks  28        Maj-AL
#> 50    2015           Joe Blanton  34        Maj-AL
#> 51    2015        Michael Blazek  26        Maj-NL
#> 52    2015     Willie Bloomquist  37        Maj-AL
#> 53    2015        Brennan Boesch  30        Maj-NL
#> 54    2015       Xander Bogaerts  22        Maj-AL
#> 55    2015        Mike Bolsinger  27        Maj-NL
#> 56    2015      Emilio Bonifácio  30        Maj-AL
#> 57    2015           Justin Bour  27        Maj-NL
#> 58    2015         Peter Bourjos  28        Maj-NL
#> 59    2015         Michael Bourn  32        Maj-AL
#> 60    2015        Archie Bradley  22        Maj-NL
#> 61    2015    Jackie Bradley Jr.  25        Maj-AL
#> 62    2015      Michael Brantley  28        Maj-AL
#> 63    2015            Ryan Braun  31        Maj-NL
#> 64    2015         Domonic Brown  27        Maj-NL
#> 65    2015             Jay Bruce  28        Maj-NL
#> 66    2015           Kris Bryant  23        Maj-NL
#> 67    2015         Clay Buchholz  30        Maj-AL
#> 68    2015          Mark Buehrle  36        Maj-AL
#> 69    2015     Madison Bumgarner  25        Maj-NL
#> 70    2015          A.J. Burnett  38        Maj-NL
#> 71    2015           Billy Burns  25        Maj-AL
#> 72    2015           Drew Butera  31        Maj-AL
#> 73    2015          Billy Butler  29        Maj-AL
#> 74    2015          Eddie Butler  24        Maj-NL
#> 75    2015           Joey Butler  29        Maj-AL
#> 76    2015          Byron Buxton  21        Maj-AL
#> 77    2015           Marlon Byrd  37        Maj-NL
#> 78    2015      Asdrúbal Cabrera  29        Maj-AL
#> 79    2015        Everth Cabrera  28        Maj-AL
#> 80    2015         Melky Cabrera  30        Maj-AL
#> 81    2015        Miguel Cabrera  32        Maj-AL
#> 82    2015         Trevor Cahill  27        Maj-NL
#> 83    2015          Lorenzo Cain  29        Maj-AL
#> 84    2015          Kole Calhoun  27        Maj-AL
#> 85    2015      Alberto Callaspo  32        Maj-NL
#> 86    2015         Eric Campbell  28        Maj-NL
#> 87    2015            Mark Canha  26        Maj-AL
#> 88    2015         Robinson Canó  32        Maj-AL
#> 89    2015          Carter Capps  24        Maj-NL
#> 90    2015         Chris Capuano  36        Maj-AL
#> 91    2015        Matt Carpenter  29        Maj-NL
#> 92    2015      Ezequiel Carrera  28        Maj-AL
#> 93    2015          Chris Carter  28        Maj-AL
#> 94    2015           Curt Casali  26        Maj-AL
#> 95    2015        Andrew Cashner  28        Maj-NL
#> 96    2015      Nick Castellanos  23        Maj-AL
#> 97    2015       Rusney Castillo  27        Maj-AL
#> 98    2015    Welington Castillo  28 Maj-AL,Maj-NL
#> 99    2015         Daniel Castro  22        Maj-NL
#> 100   2015          Jason Castro  28        Maj-AL
#> 101   2015        Starlin Castro  25        Maj-NL
#> 102   2015     Darrell Ceciliani  25        Maj-NL
#> 103   2015          Juan Centeno  25        Maj-NL
#> 104   2015    Francisco Cervelli  29        Maj-NL
#> 105   2015       Yoenis Céspedes  29        Maj-AL
#> 106   2015     Robinson Chirinos  31        Maj-AL
#> 107   2015     Lonnie Chisenhall  26        Maj-AL
#> 108   2015          Randy Choate  39        Maj-NL
#> 109   2015         Shin-Soo Choo  32        Maj-AL
#> 110   2015         Pedro Ciriaco  29        Maj-NL
#> 111   2015       Steve Clevenger  29        Maj-AL
#> 112   2015         Chris Coghlan  30        Maj-NL
#> 113   2015       Chris Colabello  31        Maj-AL
#> 114   2015             A.J. Cole  23        Maj-NL
#> 115   2015           Gerrit Cole  24        Maj-NL
#> 116   2015         Tyler Collins  25        Maj-AL
#> 117   2015       Josh Collmenter  29        Maj-NL
#> 118   2015         Bartolo Colón  42        Maj-NL
#> 119   2015       Christian Colón  26        Maj-AL
#> 120   2015           Hank Conger  27        Maj-AL
#> 121   2015        Scott Copeland  27        Maj-AL
#> 122   2015       Carlos Corporán  31        Maj-AL
#> 123   2015         Carlos Correa  20        Maj-AL
#> 124   2015         Kevin Correia  34        Maj-NL
#> 125   2015         Jarred Cosart  25        Maj-NL
#> 126   2015        Collin Cowgill  29        Maj-AL
#> 127   2015           Zack Cozart  29        Maj-NL
#> 128   2015           Tyler Cravy  25        Maj-NL
#> 129   2015      Brandon Crawford  28        Maj-NL
#> 130   2015            Coco Crisp  35        Maj-AL
#> 131   2015             C.J. Cron  25        Maj-AL
#> 132   2015           Nelson Cruz  34        Maj-AL
#> 133   2015             Tony Cruz  28        Maj-NL
#> 134   2015       Michael Cuddyer  36        Maj-NL
#> 135   2015          Johnny Cueto  29        Maj-NL
#> 136   2015       Todd Cunningham  26        Maj-NL
#> 137   2015       Travis d'Arnaud  26        Maj-NL
#> 138   2015           Chris Davis  29        Maj-AL
#> 139   2015             Ike Davis  28        Maj-AL
#> 140   2015           Khris Davis  27        Maj-NL
#> 141   2015           Rajai Davis  34        Maj-AL
#> 142   2015      Alejandro De Aza  31        Maj-AL
#> 143   2015         Iván De Jesús  28        Maj-NL
#> 144   2015      Jorge De La Rosa  34        Maj-NL
#> 145   2015      Rubby De La Rosa  26        Maj-NL
#> 146   2015          Jacob deGrom  27        Maj-NL
#> 147   2015         David DeJesus  35        Maj-AL
#> 148   2015       Randall Delgado  25        Maj-NL
#> 149   2015       Matt den Dekker  27        Maj-NL
#> 150   2015        Chris Denorfia  34        Maj-NL
#> 151   2015       Daniel Descalso  28        Maj-NL
#> 152   2015    Anthony DeSclafani  25        Maj-NL
#> 153   2015      Delino DeShields  22        Maj-AL
#> 154   2015           Ian Desmond  29        Maj-NL
#> 155   2015   Odrisamer Despaigne  28        Maj-NL
#> 156   2015       Corey Dickerson  26        Maj-NL
#> 157   2015           R.A. Dickey  40        Maj-AL
#> 158   2015        Derek Dietrich  25        Maj-NL
#> 159   2015           Wilmer Difo  23        Maj-NL
#> 160   2015       Chris Dominguez  28        Maj-NL
#> 161   2015        Josh Donaldson  29        Maj-AL
#> 162   2015            Danny Dorn  30        Maj-NL
#> 163   2015          Brian Dozier  28        Maj-AL
#> 164   2015          Stephen Drew  32        Maj-AL
#> 165   2015            Lucas Duda  29        Maj-NL
#> 166   2015            Matt Duffy  24        Maj-NL
#> 167   2015          Jarrod Dyson  30        Maj-AL
#> 168   2015             Ed Easley  29        Maj-NL
#> 169   2015            Adam Eaton  26        Maj-AL
#> 170   2015            A.J. Ellis  34        Maj-NL
#> 171   2015       Jacoby Ellsbury  31        Maj-AL
#> 172   2015           Jake Elmore  28        Maj-AL
#> 173   2015     Edwin Encarnación  32        Maj-AL
#> 174   2015        Nathan Eovaldi  25        Maj-AL
#> 175   2015       Alcides Escobar  28        Maj-AL
#> 176   2015       Eduardo Escobar  26        Maj-AL
#> 177   2015         Yunel Escobar  32        Maj-NL
#> 178   2015        Danny Espinosa  28        Maj-NL
#> 179   2015         Marco Estrada  31        Maj-AL
#> 180   2015          Andre Ethier  33        Maj-NL
#> 181   2015    Taylor Featherston  25        Maj-AL
#> 182   2015          Thomas Field  28        Maj-AL
#> 183   2015        Prince Fielder  31        Maj-AL
#> 184   2015         Daniel Fields  24        Maj-AL
#> 185   2015            Mike Fiers  30        Maj-NL
#> 186   2015           Doug Fister  31        Maj-NL
#> 187   2015         Ryan Flaherty  28        Maj-AL
#> 188   2015          Yohan Flande  29        Maj-NL
#> 189   2015          Ramón Flores  23        Maj-AL
#> 190   2015         Wilmer Flores  23        Maj-NL
#> 191   2015         Tyler Flowers  29        Maj-AL
#> 192   2015      Mike Foltynewicz  23        Maj-NL
#> 193   2015        Logan Forsythe  28        Maj-AL
#> 194   2015         Dexter Fowler  29        Maj-NL
#> 195   2015         Maikel Franco  22        Maj-NL
#> 196   2015        Jeff Francoeur  31        Maj-NL
#> 197   2015         Nick Franklin  24        Maj-AL
#> 198   2015          Todd Frazier  29        Maj-NL
#> 199   2015       Freddie Freeman  25        Maj-NL
#> 200   2015          David Freese  32        Maj-AL
#> 201   2015          Carlos Frías  25        Maj-NL
#> 202   2015              Sam Fuld  33        Maj-AL
#> 203   2015            Joey Gallo  21        Maj-AL
#> 204   2015         Freddy Galvis  25        Maj-NL
#> 205   2015         Adonis García  30        Maj-NL
#> 206   2015        Avisaíl García  24        Maj-AL
#> 207   2015           Greg Garcia  25        Maj-NL
#> 208   2015          Jaime García  28        Maj-NL
#> 209   2015         Brett Gardner  31        Maj-AL
#> 210   2015            Matt Garza  31        Maj-NL
#> 211   2015           Evan Gattis  28        Maj-AL
#> 212   2015            Dillon Gee  29        Maj-NL
#> 213   2015       Scooter Gennett  25        Maj-NL
#> 214   2015          Craig Gentry  31        Maj-AL
#> 215   2015     Johnny Giavotella  27        Maj-AL
#> 216   2015           Kyle Gibson  27        Maj-AL
#> 217   2015       Conor Gillaspie  27        Maj-AL
#> 218   2015            Ryan Goins  27        Maj-AL
#> 219   2015      Paul Goldschmidt  27        Maj-NL
#> 220   2015           Jonny Gomes  34        Maj-NL
#> 221   2015             Yan Gomes  27        Maj-AL
#> 222   2015          Carlos Gómez  29        Maj-NL
#> 223   2015          Héctor Gómez  27        Maj-NL
#> 224   2015       Adrián González  33        Maj-NL
#> 225   2015       Carlos González  29        Maj-NL
#> 226   2015          Gio Gonzalez  29        Maj-NL
#> 227   2015       Marwin González  26        Maj-AL
#> 228   2015       Miguel González  31        Maj-AL
#> 229   2015     Severino González  22        Maj-NL
#> 230   2015           Alex Gordon  31        Maj-AL
#> 231   2015          Anthony Gose  24        Maj-AL
#> 232   2015       Tuffy Gosewisch  31        Maj-NL
#> 233   2015         Phil Gosselin  26        Maj-NL
#> 234   2015       Yasmani Grandal  26        Maj-NL
#> 235   2015     Curtis Granderson  34        Maj-NL
#> 236   2015           Grant Green  27        Maj-AL
#> 237   2015          Shane Greene  26        Maj-AL
#> 238   2015        Didi Gregorius  25        Maj-AL
#> 239   2015          Zack Greinke  31        Maj-NL
#> 240   2015        Randal Grichuk  23        Maj-NL
#> 241   2015          Justin Grimm  26        Maj-NL
#> 242   2015       Robbie Grossman  25        Maj-AL
#> 243   2015         Alex Guerrero  28        Maj-NL
#> 244   2015        Jeremy Guthrie  36        Maj-AL
#> 245   2015         Brandon Guyer  29        Maj-AL
#> 246   2015           Jedd Gyorko  26        Maj-NL
#> 247   2015            Jesse Hahn  25        Maj-AL
#> 248   2015            David Hale  27        Maj-NL
#> 249   2015           Cole Hamels  31        Maj-NL
#> 250   2015        Billy Hamilton  24        Maj-NL
#> 251   2015         Josh Hamilton  34        Maj-AL
#> 252   2015          Jason Hammel  32        Maj-NL
#> 253   2015             Brad Hand  25        Maj-NL
#> 254   2015          Donovan Hand  29        Maj-NL
#> 255   2015             J.A. Happ  32        Maj-AL
#> 256   2015          Aaron Harang  37        Maj-NL
#> 257   2015            J.J. Hardy  32        Maj-AL
#> 258   2015             Dan Haren  34        Maj-NL
#> 259   2015          Bryce Harper  22        Maj-NL
#> 260   2015          Mitch Harris  29        Maj-NL
#> 261   2015         Josh Harrison  27        Maj-NL
#> 262   2015            Corey Hart  33        Maj-NL
#> 263   2015           Matt Harvey  26        Maj-NL
#> 264   2015           Brett Hayes  31        Maj-AL
#> 265   2015         Chase Headley  31        Maj-AL
#> 266   2015       Slade Heathcott  24        Maj-AL
#> 267   2015    Adeiny Hechavarría  26        Maj-NL
#> 268   2015         Austin Hedges  22        Maj-NL
#> 269   2015          Chris Heisey  30        Maj-NL
#> 270   2015     Jeremy Hellickson  28        Maj-NL
#> 271   2015        Kyle Hendricks  25        Maj-NL
#> 272   2015         Liam Hendriks  26        Maj-AL
#> 273   2015       César Hernández  25        Maj-NL
#> 274   2015     Enrique Hernández  23        Maj-NL
#> 275   2015        Dilson Herrera  21        Maj-NL
#> 276   2015         Elián Herrera  30        Maj-NL
#> 277   2015      Jonathan Herrera  30        Maj-NL
#> 278   2015        Odúbel Herrera  23        Maj-NL
#> 279   2015        Chris Herrmann  27        Maj-AL
#> 280   2015          Chris Heston  27        Maj-NL
#> 281   2015         Jason Heyward  25        Maj-NL
#> 282   2015           Aaron Hicks  25        Maj-AL
#> 283   2015            Aaron Hill  33        Maj-NL
#> 284   2015         Bryan Holaday  27        Maj-AL
#> 285   2015         Matt Holliday  35        Maj-NL
#> 286   2015            Brock Holt  27        Maj-AL
#> 287   2015           Eric Hosmer  25        Maj-AL
#> 288   2015           Ryan Howard  35        Maj-NL
#> 289   2015         Daniel Hudson  28        Maj-NL
#> 290   2015            Tim Hudson  39        Maj-NL
#> 291   2015          Nick Hundley  31        Maj-NL
#> 292   2015          Torii Hunter  39        Maj-AL
#> 293   2015        Chris Iannetta  32        Maj-AL
#> 294   2015         Jose Iglesias  25        Maj-AL
#> 295   2015       Raisel Iglesias  25        Maj-NL
#> 296   2015        Ender Inciarte  24        Maj-NL
#> 297   2015          Omar Infante  33        Maj-AL
#> 298   2015        Austin Jackson  28        Maj-AL
#> 299   2015               Jon Jay  30        Maj-NL
#> 300   2015        Ubaldo Jiménez  31        Maj-AL
#> 301   2015         Chris Johnson  30        Maj-NL
#> 302   2015         Kelly Johnson  33        Maj-NL
#> 303   2015         Micah Johnson  24        Maj-AL
#> 304   2015            Adam Jones  29        Maj-AL
#> 305   2015         Garrett Jones  34        Maj-AL
#> 306   2015           James Jones  26        Maj-AL
#> 307   2015         Taylor Jordan  26        Maj-NL
#> 308   2015          Caleb Joseph  29        Maj-AL
#> 309   2015            Matt Joyce  30        Maj-AL
#> 310   2015       Taylor Jungmann  25        Maj-NL
#> 311   2015          Jung Ho Kang  28        Maj-NL
#> 312   2015     Munenori Kawasaki  34        Maj-AL
#> 313   2015          Scott Kazmir  31        Maj-AL
#> 314   2015             Joe Kelly  27        Maj-AL
#> 315   2015             Matt Kemp  30        Maj-NL
#> 316   2015        Howie Kendrick  31        Maj-NL
#> 317   2015         Kyle Kendrick  30        Maj-NL
#> 318   2015           Ian Kennedy  30        Maj-NL
#> 319   2015       Clayton Kershaw  27        Maj-NL
#> 320   2015       Kevin Kiermaier  25        Maj-AL
#> 321   2015           Ian Kinsler  33        Maj-AL
#> 322   2015      Brandon Kintzler  30        Maj-NL
#> 323   2015          Jason Kipnis  28        Maj-AL
#> 324   2015           Tom Koehler  29        Maj-NL
#> 325   2015            Pete Kozma  27        Maj-NL
#> 326   2015           Marc Krauss  27        Maj-AL
#> 327   2015          Kyle Kubitza  24        Maj-AL
#> 328   2015           John Lackey  36        Maj-NL
#> 329   2015          Juan Lagares  26        Maj-NL
#> 330   2015           Junior Lake  25        Maj-NL
#> 331   2015             Jake Lamb  24        Maj-NL
#> 332   2015          Adam LaRoche  35        Maj-AL
#> 333   2015             Mat Latos  27        Maj-NL
#> 334   2015        Ryan Lavarnway  27 Maj-AL,Maj-NL
#> 335   2015          Brett Lawrie  25        Maj-AL
#> 336   2015            Mike Leake  27        Maj-NL
#> 337   2015           DJ LeMahieu  26        Maj-NL
#> 338   2015            Sandy León  26        Maj-AL
#> 339   2015            Jon Lester  31        Maj-NL
#> 340   2015          Tim Lincecum  31        Maj-NL
#> 341   2015             Adam Lind  31        Maj-NL
#> 342   2015      Francisco Lindor  21        Maj-AL
#> 343   2015     Francisco Liriano  31        Maj-NL
#> 344   2015          José Lobatón  30        Maj-NL
#> 345   2015            Jeff Locke  27        Maj-NL
#> 346   2015            Kyle Lohse  36        Maj-NL
#> 347   2015     Steve Lombardozzi  26        Maj-NL
#> 348   2015           James Loney  31        Maj-AL
#> 349   2015         Evan Longoria  29        Maj-AL
#> 350   2015      Michael Lorenzen  23        Maj-NL
#> 351   2015           David Lough  29        Maj-AL
#> 352   2015       Jonathan Lucroy  29        Maj-NL
#> 353   2015          Jordan Lyles  24        Maj-NL
#> 354   2015            Lance Lynn  28        Maj-NL
#> 355   2015           Tyler Lyons  27        Maj-NL
#> 356   2015         Dixon Machado  23        Maj-AL
#> 357   2015         Manny Machado  22        Maj-AL
#> 358   2015            Jean Machi  33        Maj-NL
#> 359   2015         Mikie Mahtook  25        Maj-AL
#> 360   2015      Martín Maldonado  28        Maj-NL
#> 361   2015           Seth Maness  26        Maj-NL
#> 362   2015        Jake Marisnick  24        Maj-AL
#> 363   2015         Nick Markakis  31        Maj-NL
#> 364   2015         Jason Marquis  36        Maj-NL
#> 365   2015         Alfredo Marte  26        Maj-AL
#> 366   2015        Starling Marte  26        Maj-NL
#> 367   2015         Leonys Martín  27        Maj-AL
#> 368   2015        Russell Martin  32        Maj-AL
#> 369   2015       Carlos Martínez  23        Maj-NL
#> 370   2015         J.D. Martinez  27        Maj-AL
#> 371   2015       Víctor Martínez  36        Maj-AL
#> 372   2015           Jeff Mathis  32        Maj-NL
#> 373   2015             Joe Mauer  32        Maj-AL
#> 374   2015        Justin Maxwell  31        Maj-NL
#> 375   2015            Trevor May  25        Maj-AL
#> 376   2015         John Mayberry  31        Maj-NL
#> 377   2015        Cameron Maybin  28        Maj-NL
#> 378   2015          Brian McCann  31        Maj-AL
#> 379   2015          James McCann  25        Maj-AL
#> 380   2015      Andrew McCutchen  28        Maj-NL
#> 381   2015         Casey McGehee  32        Maj-NL
#> 382   2015        Dustin McGowan  33        Maj-NL
#> 383   2015         Collin McHugh  28        Maj-AL
#> 384   2015       Michael McKenry  30        Maj-NL
#> 385   2015          Jordy Mercer  28        Maj-NL
#> 386   2015        Devin Mesoraco  27        Maj-NL
#> 387   2015     Will Middlebrooks  26        Maj-NL
#> 388   2015           Brad Miller  25        Maj-AL
#> 389   2015         Shelby Miller  24        Maj-NL
#> 390   2015         Yadier Molina  32        Maj-NL
#> 391   2015         Johnny Monell  29        Maj-NL
#> 392   2015        Miguel Montero  31        Maj-NL
#> 393   2015           Tyler Moore  28        Maj-NL
#> 394   2015       Kendrys Morales  32        Maj-AL
#> 395   2015        Mitch Moreland  29        Maj-AL
#> 396   2015        Justin Morneau  34        Maj-NL
#> 397   2015        Logan Morrison  27        Maj-AL
#> 398   2015            Mike Morse  33        Maj-NL
#> 399   2015        Charlie Morton  31        Maj-NL
#> 400   2015            Jon Moscot  23        Maj-NL
#> 401   2015          Brandon Moss  31        Maj-AL
#> 402   2015        Mike Moustakas  26        Maj-AL
#> 403   2015             Max Muncy  24        Maj-AL
#> 404   2015           Daniel Muno  26        Maj-NL
#> 405   2015         Daniel Murphy  30        Maj-NL
#> 406   2015          David Murphy  33        Maj-AL
#> 407   2015      John Ryan Murphy  24        Maj-AL
#> 408   2015             Wil Myers  24        Maj-NL
#> 409   2015           Mike Napoli  33        Maj-AL
#> 410   2015           Daniel Nava  32        Maj-AL
#> 411   2015        Dioner Navarro  31        Maj-AL
#> 412   2015         Efren Navarro  29        Maj-AL
#> 413   2015           Rey Navarro  25        Maj-AL
#> 414   2015     Kristopher Negrón  29        Maj-NL
#> 415   2015          Jimmy Nelson  26        Maj-NL
#> 416   2015       Justin Nicolino  23        Maj-NL
#> 417   2015             Jon Niese  28        Maj-NL
#> 418   2015      Kirk Nieuwenhuis  27 Maj-AL,Maj-NL
#> 419   2015         Ricky Nolasco  32        Maj-AL
#> 420   2015            Bud Norris  30        Maj-AL
#> 421   2015          Derek Norris  26        Maj-NL
#> 422   2015         Eduardo Núñez  28        Maj-AL
#> 423   2015        Vidal Nuño III  27        Maj-NL
#> 424   2015       Sean O'Sullivan  27        Maj-NL
#> 425   2015     Brett Oberholtzer  25        Maj-AL
#> 426   2015          Rougned Odor  21        Maj-AL
#> 427   2015         Jake Odorizzi  25        Maj-AL
#> 428   2015         Paulo Orlando  29        Maj-AL
#> 429   2015           David Ortiz  39        Maj-AL
#> 430   2015          Chris Owings  23        Maj-NL
#> 431   2015         Marcell Ozuna  24        Maj-NL
#> 432   2015        Jordan Pacheco  29        Maj-NL
#> 433   2015           Ángel Pagán  33        Maj-NL
#> 434   2015             Joe Panik  24        Maj-NL
#> 435   2015         Jimmy Paredes  26        Maj-AL
#> 436   2015        Jarrett Parker  26        Maj-NL
#> 437   2015           Kyle Parker  25        Maj-NL
#> 438   2015        Chris Parmelee  27        Maj-AL
#> 439   2015         Gerardo Parra  28        Maj-NL
#> 440   2015          Andy Parrino  29        Maj-AL
#> 441   2015           Ben Paulsen  27        Maj-NL
#> 442   2015          Steve Pearce  32        Maj-AL
#> 443   2015          Joc Pederson  23        Maj-NL
#> 444   2015        Dustin Pedroia  31        Maj-AL
#> 445   2015        Carlos Peguero  28        Maj-AL
#> 446   2015          Mike Pelfrey  31        Maj-AL
#> 447   2015           Brayan Peña  33        Maj-NL
#> 448   2015          Hunter Pence  32        Maj-NL
#> 449   2015      Cliff Pennington  31        Maj-NL
#> 450   2015         David Peralta  27        Maj-NL
#> 451   2015        Jhonny Peralta  33        Maj-NL
#> 452   2015          Wily Peralta  26        Maj-NL
#> 453   2015          Carlos Pérez  24        Maj-AL
#> 454   2015            Eury Pérez  25        Maj-NL
#> 455   2015          Hernán Pérez  24 Maj-AL,Maj-NL
#> 456   2015         Roberto Pérez  26        Maj-AL
#> 457   2015        Salvador Perez  25        Maj-AL
#> 458   2015        Williams Pérez  24        Maj-NL
#> 459   2015         Jace Peterson  25        Maj-NL
#> 460   2015        Shane Peterson  27        Maj-NL
#> 461   2015        Yusmeiro Petit  30        Maj-NL
#> 462   2015          Josh Phegley  27        Maj-AL
#> 463   2015          David Phelps  28        Maj-NL
#> 464   2015      Brandon Phillips  34        Maj-NL
#> 465   2015       A.J. Pierzynski  38        Maj-NL
#> 466   2015          Kevin Pillar  26        Maj-AL
#> 467   2015           José Pirela  25        Maj-AL
#> 468   2015        Kevin Plawecki  24        Maj-NL
#> 469   2015        Trevor Plouffe  29        Maj-AL
#> 470   2015       Gregory Polanco  23        Maj-NL
#> 471   2015         Jorge Polanco  21        Maj-AL
#> 472   2015            AJ Pollock  27        Maj-NL
#> 473   2015          Buster Posey  28        Maj-NL
#> 474   2015          Martín Prado  31        Maj-NL
#> 475   2015           David Price  29        Maj-AL
#> 476   2015           Yasiel Puig  24        Maj-NL
#> 477   2015         Albert Pujols  35        Maj-AL
#> 478   2015         Jose Quintana  26        Maj-AL
#> 479   2015           Ryan Raburn  34        Maj-AL
#> 480   2015        Alexei Ramírez  33        Maj-AL
#> 481   2015        Aramis Ramírez  37        Maj-NL
#> 482   2015        Erasmo Ramírez  25        Maj-AL
#> 483   2015        Hanley Ramírez  31        Maj-AL
#> 484   2015          José Ramírez  22        Maj-AL
#> 485   2015          Wilson Ramos  27        Maj-NL
#> 486   2015       Anthony Ranaudo  25        Maj-AL
#> 487   2015          Colby Rasmus  28        Maj-AL
#> 488   2015            Robbie Ray  23        Maj-NL
#> 489   2015         J.T. Realmuto  24        Maj-NL
#> 490   2015        Anthony Recker  31        Maj-NL
#> 491   2015          Josh Reddick  28        Maj-AL
#> 492   2015          Addison Reed  26        Maj-NL
#> 493   2015         Nolan Reimold  31        Maj-AL
#> 494   2015        Anthony Rendon  25        Maj-NL
#> 495   2015            Ben Revere  27        Maj-NL
#> 496   2015            José Reyes  32        Maj-AL
#> 497   2015         Mark Reynolds  31        Maj-NL
#> 498   2015          André Rienzo  26        Maj-NL
#> 499   2015             Alex Ríos  34        Maj-AL
#> 500   2015           René Rivera  31        Maj-AL
#> 501   2015         Anthony Rizzo  25        Maj-NL
#> 502   2015          Tanner Roark  28        Maj-NL
#> 503   2015      Daniel Robertson  29        Maj-AL
#> 504   2015        Clint Robinson  30        Maj-NL
#> 505   2015        Shane Robinson  30        Maj-AL
#> 506   2015          Carlos Rodón  22        Maj-AL
#> 507   2015        Alex Rodriguez  39        Maj-AL
#> 508   2015        Sean Rodríguez  30        Maj-NL
#> 509   2015       Wandy Rodríguez  36        Maj-AL
#> 510   2015          Jason Rogers  27        Maj-NL
#> 511   2015         Jimmy Rollins  36        Maj-NL
#> 512   2015         Andrew Romine  29        Maj-AL
#> 513   2015           Sergio Romo  32        Maj-NL
#> 514   2015          Adam Rosales  32        Maj-AL
#> 515   2015         Eddie Rosario  23        Maj-AL
#> 516   2015         Wilin Rosario  26        Maj-NL
#> 517   2015            David Ross  38        Maj-NL
#> 518   2015              Joe Ross  22        Maj-NL
#> 519   2015            Tyson Ross  28        Maj-NL
#> 520   2015              Ryan Rua  25        Maj-AL
#> 521   2015             Darin Ruf  28        Maj-NL
#> 522   2015       Justin Ruggiano  33        Maj-AL
#> 523   2015           Carlos Ruiz  36        Maj-NL
#> 524   2015          Cameron Rupp  26        Maj-NL
#> 525   2015           Chris Rusin  28        Maj-NL
#> 526   2015       Addison Russell  21        Maj-NL
#> 527   2015          Brendan Ryan  33        Maj-AL
#> 528   2015            Chris Sale  26        Maj-AL
#> 529   2015 Jarrod Saltalamacchia  30        Maj-NL
#> 530   2015       Jeff Samardzija  30        Maj-AL
#> 531   2015        Yolmer Sanchez  23        Maj-AL
#> 532   2015        Pablo Sandoval  28        Maj-AL
#> 533   2015           Jerry Sands  27        Maj-AL
#> 534   2015        Carlos Santana  29        Maj-AL
#> 535   2015         Danny Santana  24        Maj-AL
#> 536   2015       Domingo Santana  22        Maj-AL
#> 537   2015       Héctor Santiago  27        Maj-AL
#> 538   2015         Luis Sardiñas  22        Maj-NL
#> 539   2015        Jordan Schafer  28        Maj-AL
#> 540   2015        Scott Schebler  24        Maj-NL
#> 541   2015          Max Scherzer  30        Maj-NL
#> 542   2015        Skip Schumaker  35        Maj-NL
#> 543   2015        Kyle Schwarber  22        Maj-NL
#> 544   2015        Xavier Scruggs  27        Maj-NL
#> 545   2015           Kyle Seager  27        Maj-AL
#> 546   2015           Jean Segura  25        Maj-NL
#> 547   2015         Marcus Semien  24        Maj-AL
#> 548   2015           Travis Shaw  25        Maj-AL
#> 549   2015         James Shields  33        Maj-NL
#> 550   2015              JB Shuck  28        Maj-AL
#> 551   2015     Andrelton Simmons  25        Maj-NL
#> 552   2015         Alfredo Simón  34        Maj-AL
#> 553   2015        Grady Sizemore  32        Maj-NL
#> 554   2015            Seth Smith  32        Maj-AL
#> 555   2015          Justin Smoak  28        Maj-AL
#> 556   2015        Jake Smolinski  26        Maj-AL
#> 557   2015         Travis Snider  27        Maj-AL
#> 558   2015     Miguel Socolovich  28        Maj-NL
#> 559   2015           Eric Sogard  29        Maj-AL
#> 560   2015        Donovan Solano  27        Maj-NL
#> 561   2015       Jhonatan Solano  29        Maj-NL
#> 562   2015     Yangervis Solarte  27        Maj-NL
#> 563   2015           Jorge Soler  23        Maj-NL
#> 564   2015           Sammy Solís  26        Maj-NL
#> 565   2015          Geovany Soto  32        Maj-AL
#> 566   2015      Steven Souza Jr.  26        Maj-AL
#> 567   2015           Denard Span  31        Maj-NL
#> 568   2015      Cory Spangenberg  24        Maj-NL
#> 569   2015       George Springer  25        Maj-AL
#> 570   2015     Giancarlo Stanton  25        Maj-NL
#> 571   2015         Chris Stewart  33        Maj-NL
#> 572   2015    Dee Strange-Gordon  27        Maj-NL
#> 573   2015     Stephen Strasburg  26        Maj-NL
#> 574   2015     Hunter Strickland  26        Maj-NL
#> 575   2015           Drew Stubbs  30        Maj-NL
#> 576   2015           Eric Stults  35        Maj-NL
#> 577   2015        Eugenio Suárez  23        Maj-NL
#> 578   2015           Jesús Sucre  27        Maj-AL
#> 579   2015          Andrew Susac  25        Maj-NL
#> 580   2015         Ichiro Suzuki  41        Maj-NL
#> 581   2015           Kurt Suzuki  31        Maj-AL
#> 582   2015         Blake Swihart  23        Maj-AL
#> 583   2015          Nick Swisher  34        Maj-AL
#> 584   2015      Noah Syndergaard  22        Maj-NL
#> 585   2015           Matt Szczur  25        Maj-NL
#> 586   2015           José Tábata  26        Maj-NL
#> 587   2015       Masahiro Tanaka  26        Maj-AL
#> 588   2015          Chris Taylor  24        Maj-AL
#> 589   2015     Michael A. Taylor  24        Maj-NL
#> 590   2015         Julio Teheran  24        Maj-NL
#> 591   2015         Mark Teixeira  35        Maj-AL
#> 592   2015          Rubén Tejada  25        Maj-NL
#> 593   2015     Joey Terdoslavich  26        Maj-NL
#> 594   2015            Josh Thole  28        Maj-AL
#> 595   2015            Ian Thomas  28        Maj-NL
#> 596   2015       Steven Tolleson  31        Maj-AL
#> 597   2015         Yasmany Tomás  24        Maj-NL
#> 598   2015          Devon Travis  24        Maj-AL
#> 599   2015         Blake Treinen  27        Maj-NL
#> 600   2015            Mike Trout  23        Maj-AL
#> 601   2015           Mark Trumbo  29 Maj-AL,Maj-NL
#> 602   2015        Preston Tucker  24        Maj-AL
#> 603   2015       Troy Tulowitzki  30        Maj-NL
#> 604   2015         Justin Turner  30        Maj-NL
#> 605   2015             Dan Uggla  35        Maj-NL
#> 606   2015            B.J. Upton  30        Maj-NL
#> 607   2015          Justin Upton  27        Maj-NL
#> 608   2015            José Ureña  23        Maj-NL
#> 609   2015            Juan Uribe  36        Maj-NL
#> 610   2015           Gio Urshela  23        Maj-AL
#> 611   2015           Chase Utley  36        Maj-NL
#> 612   2015         Luis Valbuena  29        Maj-AL
#> 613   2015        Danny Valencia  30        Maj-AL
#> 614   2015       Scott Van Slyke  28        Maj-NL
#> 615   2015         Kennys Vargas  24        Maj-AL
#> 616   2015          Will Venable  32        Maj-NL
#> 617   2015       Yordano Ventura  24        Maj-AL
#> 618   2015       Shane Victorino  34        Maj-AL
#> 619   2015     Carlos Villanueva  31        Maj-NL
#> 620   2015       Jonathan Villar  24        Maj-AL
#> 621   2015        Ryan Vogelsong  37        Maj-NL
#> 622   2015          Stephen Vogt  30        Maj-AL
#> 623   2015       Edinson Vólquez  31        Maj-AL
#> 624   2015            Joey Votto  31        Maj-NL
#> 625   2015         Michael Wacha  23        Maj-NL
#> 626   2015         Tsuyoshi Wada  34        Maj-NL
#> 627   2015          Tyler Wagner  24        Maj-NL
#> 628   2015           Neil Walker  29        Maj-NL
#> 629   2015        Taijuan Walker  22        Maj-AL
#> 630   2015         Brett Wallace  28        Maj-NL
#> 631   2015          Zach Walters  25        Maj-AL
#> 632   2015           Adam Warren  27        Maj-AL
#> 633   2015         Allen Webster  25        Maj-NL
#> 634   2015          Rickie Weeks  32        Maj-AL
#> 635   2015          Jayson Werth  36        Maj-NL
#> 636   2015          Matt Wieters  29        Maj-AL
#> 637   2015       Jerome Williams  33        Maj-NL
#> 638   2015        Mason Williams  23        Maj-AL
#> 639   2015          Bobby Wilson  32        Maj-AL
#> 640   2015           C.J. Wilson  34        Maj-AL
#> 641   2015           Josh Wilson  34        Maj-AL
#> 642   2015          Tyler Wilson  25        Maj-AL
#> 643   2015           Matt Wisler  22        Maj-NL
#> 644   2015           Kolten Wong  24        Maj-NL
#> 645   2015             Alex Wood  24        Maj-NL
#> 646   2015           Travis Wood  28        Maj-NL
#> 647   2015          Vance Worley  27        Maj-NL
#> 648   2015       Mike Wright Jr.  25        Maj-AL
#> 649   2015      Christian Yelich  23        Maj-NL
#> 650   2015           Rafael Ynoa  27        Maj-NL
#> 651   2015           Chris Young  36        Maj-AL
#> 652   2015           Chris Young  31        Maj-AL
#> 653   2015          Delmon Young  29        Maj-AL
#> 654   2015        Eric Young Jr.  30        Maj-NL
#> 655   2015        Ryan Zimmerman  30        Maj-NL
#> 656   2015     Jordan Zimmermann  29        Maj-NL
#> 657   2015           Ben Zobrist  34        Maj-AL
#> 658   2015           Mike Zunino  24        Maj-AL
#>                        Team  G  PA  AB  R  H X1B X2B X3B HR RBI BB IBB
#> 1                   Chicago 36 151 140 22 41  29   5   1  6  23  8   1
#> 2                   Seattle 28  78  67  6 12   8   3   0  1   6  8   0
#> 3                 St. Louis 15  53  50  3 10   6   3   0  1   5  3   0
#> 4             San Francisco  1   1   1  0  0   0   0   0  0   0  0   0
#> 5                   Arizona 35 123 109 18 32  23   5   1  3  13 12   1
#> 6                     Texas 18  65  61  9 17  14   2   1  0   3  2   0
#> 7                 San Diego 22  48  42  6  8   6   2   0  0   4  4   0
#> 8                 San Diego 18  71  62  5 18  14   3   0  1   8  8   0
#> 9                   Houston 32 132 125 11 28  24   3   0  1   7  4   1
#> 10                    Miami  2   3   3  0  1   1   0   0  0   1  0   0
#> 11               Pittsburgh 33 116 106 13 26  16   5   0  5  14  9   3
#> 12                San Diego 30  94  86  9 21  15   4   1  1   8  5   1
#> 13              Los Angeles  6  13   9  0  1   0   1   0  0   2  3   0
#> 14                  Arizona  7  18  12  0  2   2   0   0  0   1  0   0
#> 15                Tampa Bay  1   1   1  0  0   0   0   0  0   0  0   0
#> 16                    Texas 38 158 137 15 33  26   4   1  2  16 14   0
#> 17            San Francisco 36 149 135 20 48  44   2   1  1  11 10   0
#> 18                Tampa Bay  1   2   2  0  0   0   0   0  0   0  0   0
#> 19                 Colorado 41 169 156 23 42  22   7   3 10  36  6   1
#> 20            San Francisco 14  22  22  2  2   2   0   0  0   2  0   0
#> 21                  Chicago  6  14  14  0  0   0   0   0  0   0  0   0
#> 22             Philadelphia 22  81  78  7 17  11   5   0  1   3  3   0
#> 23             Philadelphia  1   1   0  0  0   0   0   0  0   0  1   0
#> 24                Cleveland 25  86  78  7 21  18   2   0  1   4  5   0
#> 25              Los Angeles 35 151 140 24 38  30   7   0  1  10  8   0
#> 26                    Miami 18  45  41  5 10   6   1   0  3   5  4   0
#> 27                San Diego 23  60  56  6 18  13   3   1  1   6  1   0
#> 28              Los Angeles  4   6   5  1  1   1   0   0  0   0  1   0
#> 29                 Colorado 30  99  89 10 26  19   7   0  0   6  8   0
#> 30               Cincinnati 13  46  42  4  9   7   1   0  1   3  3   0
#> 31                Cleveland  1   3   3  0  1   1   0   0  0   0  0   0
#> 32                  Toronto 37 160 124 25 35  19   7   1  8  27 30   2
#> 33                  Chicago 22  32  28  4  7   6   1   0  0   1  3   0
#> 34                  Chicago 32 104  95  8 18  13   4   0  1   7  7   0
#> 35                Tampa Bay 11  29  26  4  6   3   0   1  2   4  1   0
#> 36                Tampa Bay  1   1   1  0  0   0   0   0  0   0  0   0
#> 37            San Francisco 38 156 141 25 36  17  10   0  9  24 13   1
#> 38                 New York 31 123 112 12 34  22   5   0  7  17  9   0
#> 39                    Texas 21  90  83 12 25  18   3   0  4  12  4   1
#> 40                Minnesota  3   5   5  0  1   0   1   0  0   2  0   0
#> 41                  Atlanta 15  54  53  7 11   8   2   0  1   5  1   1
#> 42                 Colorado  7  16  14  0  0   0   0   0  0   1  2   0
#> 43                   Boston 37 153 142 13 42  31   6   3  2  13  8   0
#> 44                   Boston  2   2   2  0  0   0   0   0  0   0  0   0
#> 45             Philadelphia  2   4   4  1  1   0   0   0  1   1  0   0
#> 46                 Colorado 40 175 153 24 37  27   5   1  4  12 12   1
#> 47             Philadelphia 24  33  28  2  7   2   4   0  1   2  4   0
#> 48            San Francisco 24  74  66 15 23  15   7   0  1   7  7   2
#> 49                    Texas  8  29  28  2  8   5   3   0  0   1  1   0
#> 50              Kansas City  1   1   1  0  0   0   0   0  0   0  0   0
#> 51                Milwaukee  2   2   2  0  1   0   1   0  0   1  0   0
#> 52                  Seattle 21  43  42  1  6   5   1   0  0   3  1   0
#> 53               Cincinnati 18  32  31  1  3   3   0   0  0   1  1   0
#> 54                   Boston 37 143 135 15 41  32   6   1  2  16  4   0
#> 55              Los Angeles  8  19  18  0  0   0   0   0  0   0  0   0
#> 56                  Chicago 22  47  45  4  8   7   1   0  0   3  1   0
#> 57                    Miami 32 104  93  8 20  14   2   0  4   9 10   1
#> 58                St. Louis 29  87  76 10 15  10   2   1  2   4  9   1
#> 59                Cleveland 32 112  98 10 26  22   3   1  0   7  9   0
#> 60                  Arizona  4   6   6  0  1   1   0   0  0   0  0   0
#> 61                   Boston  4  13  11  0  0   0   0   0  0   0  2   0
#> 62                Cleveland 35 156 134 12 36  22  13   0  1  19 19   2
#> 63                Milwaukee 37 152 136 17 33  17   7   1  8  27 12   1
#> 64             Philadelphia  6  24  20  0  3   3   0   0  0   1  4   0
#> 65               Cincinnati 36 149 128 14 36  22   9   0  5  16 21   2
#> 66                  Chicago 37 165 143 25 42  26   7   2  7  23 17   0
#> 67                   Boston  1   3   3  0  0   0   0   0  0   0  0   0
#> 68                  Toronto  2   6   5  0  1   1   0   0  0   0  0   0
#> 69            San Francisco  6  16  14  2  4   3   0   0  1   2  2   0
#> 70               Pittsburgh  8  21  18  0  1   1   0   0  0   1  0   0
#> 71                  Oakland 37 172 159 25 52  42   4   4  2  14  9   0
#> 72              Kansas City  6  14  13  1  2   1   1   0  0   0  0   0
#> 73                  Oakland 37 152 137 12 34  27   5   0  2  16 11   0
#> 74                 Colorado  4   7   6  0  0   0   0   0  0   0  0   0
#> 75                Tampa Bay 35 128 122 16 43  29   9   0  5  15  4   0
#> 76                Minnesota  7  24  22  2  2   1   0   1  0   0  2   0
#> 77               Cincinnati 21  81  71 11 13   5   3   0  5   9  8   0
#> 78                Tampa Bay 35 130 118 10 24  16   4   1  3   7 10   3
#> 79                Baltimore  6  16  13  1  3   3   0   0  0   0  3   0
#> 80                  Chicago 39 167 158 10 33  28   5   0  0  12  5   0
#> 81                  Detroit 36 153 129 22 45  28   7   1  9  30 23   6
#> 82                  Atlanta  1   1   1  0  1   1   0   0  0   0  0   0
#> 83              Kansas City 33 136 125 19 31  21   6   1  3  17  9   0
#> 84              Los Angeles 38 160 145 12 34  26   5   0  3  15 12   1
#> 85      Atlanta,Los Angeles 27  82  72  7 16  13   3   0  0   5  9   0
#> 86                 New York 24  77  71  6 11   8   2   0  1   7  5   0
#> 87                  Oakland 25  88  76 11 18  12   3   0  3  11  8   0
#> 88                  Seattle 36 151 142 10 31  23   7   0  1  12  7   0
#> 89                    Miami  2   2   2  0  1   1   0   0  0   0  0   0
#> 90                 New York  1   1   1  0  0   0   0   0  0   0  0   0
#> 91                St. Louis 36 153 126 20 33  24   6   0  3  13 23   1
#> 92                  Toronto 27  80  69  9 17  13   4   0  0   7  5   0
#> 93                  Houston 39 154 124 16 31  16   8   0  7  23 24   1
#> 94                Tampa Bay  3   7   6  1  3   2   0   0  1   1  1   0
#> 95                San Diego  7  14  14  0  1   1   0   0  0   0  0   0
#> 96                  Detroit 36 137 127  6 27  21   3   1  2  15  8   0
#> 97                   Boston 24  76  73  6 17  15   1   0  1   6  3   0
#> 98  Arizona,Chicago,Seattle 23  76  69  9 13   8   3   0  2   7  4   0
#> 99                  Atlanta  1   1   1  0  1   1   0   0  0   0  0   0
#> 100                 Houston 27  95  87 11 20  11   4   0  5  10  6   0
#> 101                 Chicago 36 157 146 12 35  28   4   1  2  15  7   2
#> 102                New York 26  50  46  4 12   9   2   0  1   3  2   0
#> 103               Milwaukee  6  14  12  0  1   0   1   0  0   0  2   0
#> 104              Pittsburgh 30 119 101 14 36  29   4   1  2  15 15   0
#> 105                 Detroit 37 150 139 24 44  30   9   0  5  18  9   0
#> 106                   Texas 25  92  80 12 18   9   4   1  4  13  8   0
#> 107               Cleveland 24  83  81  8 14   7   5   0  2   8  1   0
#> 108               St. Louis  1   1   0  0  0   0   0   0  0   0  1   0
#> 109                   Texas 38 172 155 24 42  31   5   1  5  20 15   0
#> 110                 Atlanta 21  46  43  6 12   8   3   1  0   7  0   0
#> 111               Baltimore  4  11  11  1  5   4   1   0  0   1  0   0
#> 112                 Chicago 37 127 111 17 32  21   6   0  5  14 14   4
#> 113                 Toronto 38 159 149 23 49  37   8   0  4  21  9   0
#> 114              Washington  1   1   1  0  0   0   0   0  0   0  0   0
#> 115              Pittsburgh  7  18  14  3  2   2   0   0  0   0  1   0
#> 116                 Detroit 17  54  51  7 14   9   1   2  2   7  3   0
#> 117                 Arizona  5  11   8  1  1   1   0   0  0   0  2   0
#> 118                New York  7  14  13  0  3   2   1   0  0   1  0   0
#> 119             Kansas City  7  17  14  1  3   2   1   0  0   0  3   0
#> 120                 Houston 17  56  50  5 11   5   4   0  2   7  6   0
#> 121                 Toronto  1   1   1  0  0   0   0   0  0   0  0   0
#> 122                   Texas 16  58  54  4 10   8   1   0  1   7  1   0
#> 123                 Houston 12  53  51  7 16   9   4   0  3   7  2   0
#> 124            Philadelphia  2   3   3  0  1   1   0   0  0   0  0   0
#> 125                   Miami  1   2   2  0  1   1   0   0  0   1  0   0
#> 126             Los Angeles  8  14  14  2  2   2   0   0  0   0  0   0
#> 127              Cincinnati 26 106  96 12 20  11   5   0  4  15  8   1
#> 128               Milwaukee  1   2   1  0  0   0   0   0  0   0  0   0
#> 129           San Francisco 37 150 140 20 42  26  11   1  4  26  6   0
#> 130                 Oakland  9  37  31  3  2   1   1   0  0   0  6   0
#> 131             Los Angeles 13  31  29  1  6   6   0   0  0   0  0   0
#> 132                 Seattle 37 153 136 16 39  31   3   0  5  16 17   2
#> 133               St. Louis 20  28  26  0  5   5   0   0  0   0  1   0
#> 134                New York 35 142 133 13 36  26   7   0  3  16  8   0
#> 135              Cincinnati  5  14  12  1  3   3   0   0  0   0  0   0
#> 136                 Atlanta 25  77  71  9 17  13   4   0  0   4  4   1
#> 137                New York  8  32  30  6  8   4   2   0  2   7  2   0
#> 138               Baltimore 39 161 137 18 27  13   7   0  7  20 22   0
#> 139                 Oakland  5  14  13  0  2   1   1   0  0   0  1   0
#> 140               Milwaukee 17  55  49  7 11   5   1   2  3   5  6   0
#> 141                 Detroit 26  85  80 10 23  12   7   4  0   5  4   0
#> 142        Baltimore,Boston 24  75  68  8 17  10   5   2  0   6  6   2
#> 143              Cincinnati  8  33  32  3  7   3   2   0  2   6  1   0
#> 144                Colorado  7  15  14  0  1   1   0   0  0   2  0   0
#> 145                 Arizona  8  21  19  1  2   2   0   0  0   1  0   0
#> 146                New York  8  22  19  1  4   4   0   0  0   1  0   0
#> 147               Tampa Bay 33  99  90  9 27  19   4   1  3  11  7   1
#> 148                 Arizona  1   1   1  0  0   0   0   0  0   0  0   0
#> 149              Washington  3   2   2  0  0   0   0   0  0   0  0   0
#> 150                 Chicago 12  40  37  3 11   8   2   0  1   6  1   0
#> 151                Colorado 25  57  53  4 17  14   1   0  2  11  2   1
#> 152              Cincinnati  7  15  14  1  3   3   0   0  0   0  0   0
#> 153                   Texas 32 133 115 25 33  22   7   4  0   7 15   1
#> 154              Washington 37 153 144 12 31  21   7   0  3  11  5   0
#> 155               San Diego  4   9   8  0  1   0   1   0  0   0  0   0
#> 156                Colorado 12  31  31  0  9   5   4   0  0   1  0   0
#> 157                 Toronto  1   2   2  0  0   0   0   0  0   0  0   0
#> 158                   Miami  8  23  19  5  6   1   2   0  3   5  2   0
#> 159              Washington  5   5   5  0  1   1   0   0  0   0  0   0
#> 160              Cincinnati  6  14  14  2  4   1   1   1  1   2  0   0
#> 161                 Toronto 39 174 155 30 45  25  10   0 10  26 13   0
#> 162                 Arizona  3  10  10  0  2   2   0   0  0   0  0   0
#> 163               Minnesota 37 160 140 30 38  14  12   2 10  17 15   0
#> 164                New York 35 130 122 12 22  12   5   0  5  12  7   0
#> 165                New York 37 160 132 22 36  17  11   0  8  18 19   2
#> 166           San Francisco 35 139 128 15 38  27   5   1  5  24  7   0
#> 167             Kansas City 13  33  31  2 10   6   2   2  0   2  0   0
#> 168               St. Louis  2   3   2  0  0   0   0   0  0   1  0   0
#> 169                 Chicago 38 167 151 26 43  32   5   3  3  11 12   0
#> 170             Los Angeles 14  46  37  3  7   5   2   0  0   3  7   0
#> 171                New York  8  33  27  4  6   5   1   0  0   2  6   0
#> 172               Tampa Bay 21  77  70  3 19  16   3   0  0  10  4   0
#> 173                 Toronto 34 138 116 22 28  14   6   0  8  23 19   0
#> 174                New York  1   2   2  0  0   0   0   0  0   0  0   0
#> 175             Kansas City 35 155 144 21 37  28   6   2  1  16  4   0
#> 176               Minnesota 31 106 100  5 24  16   5   2  1   9  4   0
#> 177              Washington 35 149 136 20 47  40   5   1  1  13  8   0
#> 178              Washington 36 135 120 21 33  21   7   1  4  12 13   3
#> 179                 Toronto  1   3   3  0  1   1   0   0  0   0  0   0
#> 180             Los Angeles 38 130 120 13 32  22   3   2  5  17 10   0
#> 181             Los Angeles 19  29  28  5  3   0   2   0  1   2  1   0
#> 182                   Texas 14  45  41  6  8   5   1   0  2   5  2   0
#> 183                   Texas 37 163 143 23 48  33   6   0  9  33 13   6
#> 184                 Detroit  1   3   3  1  1   0   1   0  0   0  0   0
#> 185               Milwaukee  5  10  10  0  1   1   0   0  0   0  0   0
#> 186              Washington  2   3   1  0  1   1   0   0  0   0  0   0
#> 187               Baltimore 26  89  80 11 18  14   1   2  1   8  6   0
#> 188                Colorado  2   2   1  0  0   0   0   0  0   0  0   0
#> 189                New York  7  26  26  3  7   6   1   0  0   0  0   0
#> 190                New York 39 155 147 17 37  24   6   0  7  24  4   0
#> 191                 Chicago 25  91  83  6 19  12   4   0  3   8  6   0
#> 192                 Atlanta  7  18  17  0  1   1   0   0  0   0  0   0
#> 193               Tampa Bay 39 158 138 19 38  27   6   0  5  19 12   0
#> 194                 Chicago 37 170 148 28 32  23   2   2  5  11 18   0
#> 195            Philadelphia 34 140 131 17 37  22   7   1  7  18  8   1
#> 196            Philadelphia 29  80  77  6 23  16   5   0  2  13  2   0
#> 197               Tampa Bay 25  75  69  8 10   5   3   1  1   3  6   0
#> 198              Cincinnati 37 158 148 27 49  21  16   0 12  27  8   0
#> 199                 Atlanta 36 158 139 22 39  25   7   0  7  23 16   3
#> 200             Los Angeles 35 137 124 16 31  17   9   0  5  15 10   0
#> 201             Los Angeles  7  14  11  0  1   1   0   0  0   0  1   0
#> 202                 Oakland 27  68  59  4 12   9   3   0  0   5  8   0
#> 203                   Texas 17  67  59 10 13   7   1   0  5  10  8   2
#> 204            Philadelphia 34 135 128 14 25  22   2   0  1   8  6   0
#> 205                 Atlanta  2   2   1  0  0   0   0   0  0   0  1   0
#> 206                 Chicago 32 128 117 11 27  20   2   0  5  16  8   0
#> 207               St. Louis  1   2   2  0  2   2   0   0  0   0  0   0
#> 208               St. Louis  5  11  11  0  1   1   0   0  0   1  0   0
#> 209                New York 37 163 147 27 38  22   8   3  5  20 13   0
#> 210               Milwaukee  7  12  11  0  2   2   0   0  0   0  0   0
#> 211                 Houston 37 145 137 22 37  23   5   2  7  22  6   1
#> 212                New York  2   2   2  0  0   0   0   0  0   0  0   0
#> 213               Milwaukee 15  53  52  2 10   5   3   1  1   4  1   0
#> 214                 Oakland  1   3   3  0  0   0   0   0  0   0  0   0
#> 215             Los Angeles 36 136 120 14 33  25   5   1  2  13 11   0
#> 216               Minnesota  1   2   2  0  1   1   0   0  0   0  0   0
#> 217                 Chicago 24  77  73  5 18  14   2   0  2   6  4   1
#> 218                 Toronto 38 125 112 15 26  17   7   0  2  17  8   0
#> 219                 Arizona 39 169 133 26 48  29   8   1 10  28 35  14
#> 220                 Atlanta 26  73  66 10 14  11   3   0  0   2  6   0
#> 221               Cleveland 20  78  73  5 16  12   2   0  2   6  2   0
#> 222               Milwaukee 31 136 127 19 37  24   8   1  4  15  6   0
#> 223               Milwaukee 27  66  63  7 12   3   8   1  0   2  2   0
#> 224             Los Angeles 39 161 142 14 32  20  10   0  2  19 16   3
#> 225                Colorado 37 157 139 19 37  24   5   0  8  21 15   1
#> 226              Washington  5  10   7  0  0   0   0   0  0   0  0   0
#> 227                 Houston 28  95  90 11 19  12   4   0  3   9  3   0
#> 228               Baltimore  1   1   1  0  0   0   0   0  0   0  0   0
#> 229            Philadelphia  2   4   3  1  0   0   0   0  0   0  1   0
#> 230             Kansas City 34 133 109 11 28  17   6   0  5  16 18   2
#> 231                 Detroit 34 142 133 16 33  26   5   2  0   4  8   0
#> 232                 Arizona 13  54  48  4  9   5   3   0  1   4  5   0
#> 233                 Atlanta  6  15  15  1  7   5   2   0  0   0  0   0
#> 234             Los Angeles 29 107  90  8 21  15   2   0  4  11 17   0
#> 235                New York 40 166 148 22 36  24   5   0  7  13 14   1
#> 236             Los Angeles  8  15  14  2  3   3   0   0  0   2  1   1
#> 237                 Detroit  1   2   2  0  0   0   0   0  0   0  0   0
#> 238                New York 36 127 117 16 30  20   6   0  4  11  7   0
#> 239             Los Angeles  8  16  13  1  3   3   0   0  0   0  1   0
#> 240               St. Louis 32 114 109 16 33  15   9   4  5  16  4   1
#> 241                 Chicago  1   1   1  0  0   0   0   0  0   0  0   0
#> 242                 Houston  2   5   5  0  0   0   0   0  0   0  0   0
#> 243             Los Angeles 33  87  84  9 21  11   5   1  4  15  2   0
#> 244             Kansas City  1   2   1  0  0   0   0   0  0   0  0   0
#> 245               Tampa Bay 30  86  79  8 21  18   2   1  0   5  2   0
#> 246               San Diego 22  54  48  3 11   9   1   0  1   4  6   1
#> 247                 Oakland  1   3   3  0  0   0   0   0  0   0  0   0
#> 248                Colorado  5  13  11  0  0   0   0   0  0   0  0   0
#> 249            Philadelphia  7  18  17  0  1   1   0   0  0   0  0   0
#> 250              Cincinnati 32 126 115 11 27  24   1   1  1  13  5   0
#> 251                   Texas  7  26  22  5  6   2   2   0  2   5  4   0
#> 252                 Chicago  6  16  16  1  3   3   0   0  0   1  0   0
#> 253                   Miami  2   4   4  1  1   1   0   0  0   0  0   0
#> 254              Cincinnati  1   1   1  0  0   0   0   0  0   0  0   0
#> 255                 Seattle  1   2   1  0  0   0   0   0  0   0  1   0
#> 256            Philadelphia  7  15  13  0  3   2   1   0  0   0  0   0
#> 257               Baltimore 35 134 129 13 32  27   3   0  2  12  3   0
#> 258                   Miami  8  15  10  0  1   1   0   0  0   0  1   0
#> 259              Washington 35 145 116 26 46  24   9   1 12  28 27   2
#> 260               St. Louis  1   1   1  0  0   0   0   0  0   0  0   0
#> 261              Pittsburgh 35 157 144 22 51  39  10   0  2  16  7   0
#> 262              Pittsburgh 16  27  26  0  6   6   0   0  0   2  0   0
#> 263                New York  7  15  15  0  2   1   1   0  0   1  0   0
#> 264               Cleveland  3   9   7  2  1   0   0   0  1   1  2   0
#> 265                New York 37 158 143 22 39  31   5   0  3  15 10   0
#> 266                New York  6  18  17  3  6   4   1   0  1   3  0   0
#> 267                   Miami 36 136 127 14 33  26   4   2  1   9  8   2
#> 268               San Diego 12  29  28  2  3   1   1   0  1   1  0   0
#> 269             Los Angeles 12  29  24  3  4   3   1   0  0   0  5   0
#> 270                 Arizona  6  13  10  1  3   3   0   0  0   2  0   0
#> 271                 Chicago  7  18  16  1  1   0   1   0  0   0  1   0
#> 272                 Toronto  1   1   0  0  0   0   0   0  0   0  1   0
#> 273            Philadelphia 30  77  70 10 16   9   6   0  1   6  6   0
#> 274             Los Angeles 26  65  59  9 15  10   3   0  2   6  4   0
#> 275                New York 13  49  42  2  8   6   1   0  1   1  6   0
#> 276               Milwaukee 19  57  52  4 12   7   2   0  3   9  3   0
#> 277                 Chicago 15  22  21  3  5   5   0   0  0   2  0   0
#> 278            Philadelphia 31 106  98  8 22  12   8   0  2   8  4   0
#> 279               Minnesota  9  29  25  4  5   2   2   0  1   3  2   0
#> 280           San Francisco  7  16  15  1  5   4   1   0  0   2  0   0
#> 281               St. Louis 33 125 117 12 34  23   7   0  4  15  6   1
#> 282               Minnesota 28  99  93  9 23  20   2   0  1   2  6   0
#> 283                 Arizona 32  89  80  6 17  13   3   0  1   8  7   0
#> 284                 Detroit 11  31  31  1  8   4   3   0  1  10  0   0
#> 285               St. Louis 24  95  79  5 19  14   3   1  1   9 11   0
#> 286                  Boston 32 127 108 19 33  20  10   2  1   8 18   0
#> 287             Kansas City 35 139 125 19 34  25   6   0  3  13 13   3
#> 288            Philadelphia 37 145 138 14 32  15  10   0  7  18  5   0
#> 289                 Arizona  1   1   1  0  0   0   0   0  0   0  0   0
#> 290           San Francisco  7  15  15  2  3   2   1   0  0   0  0   0
#> 291                Colorado 30 112 106 15 26  19   4   0  3  14  5   0
#> 292               Minnesota 32 134 119 13 27  21   3   0  3  17 13   1
#> 293             Los Angeles 23  78  69  8 18  12   3   0  3  13  9   0
#> 294                 Detroit 32 118 109  5 33  31   2   0  0   6  6   1
#> 295              Cincinnati  2   5   5  0  0   0   0   0  0   0  0   0
#> 296                 Arizona 34 150 139 19 38  32   3   1  2  10  5   0
#> 297             Kansas City 34 125 122 16 26  20   5   1  0   6  1   0
#> 298                 Seattle 24 102  95  9 26  17   6   2  1   9  6   0
#> 299               St. Louis 21  52  45  6  9   7   0   1  1   4  4   2
#> 300               Baltimore  2   4   4  0  1   1   0   0  0   1  0   0
#> 301                 Atlanta 14  35  34  2  5   4   0   0  1   2  1   0
#> 302                 Atlanta 13  45  43  2 12  10   2   0  0   4  2   0
#> 303                 Chicago  3   6   4  1  1   1   0   0  0   1  1   0
#> 304               Baltimore 35 148 138 18 35  24   4   2  5  15  8   2
#> 305                New York 23  53  48  6 14  11   0   0  3   9  5   0
#> 306                 Seattle  3   6   4  0  0   0   0   0  0   0  2   0
#> 307              Washington  2   5   5  0  2   2   0   0  0   0  0   0
#> 308               Baltimore 28  93  84 11 15   9   4   0  2  12  5   1
#> 309             Los Angeles 33 117  98 10 22  11   7   1  3  11 16   0
#> 310               Milwaukee  3   6   5  0  1   1   0   0  0   0  1   0
#> 311              Pittsburgh 34 134 118 13 31  22   6   0  3  18 10   0
#> 312                 Toronto  8  13  11  2  3   1   2   0  0   1  2   0
#> 313                 Oakland  1   2   2  1  1   1   0   0  0   1  0   0
#> 314                  Boston  1   2   2  0  1   1   0   0  0   1  0   0
#> 315               San Diego 38 157 148 17 33  24   6   0  3  15  8   0
#> 316             Los Angeles 37 154 145 19 42  34   5   0  3  13  9   0
#> 317                Colorado  7  15  15  0  1   1   0   0  0   0  0   0
#> 318               San Diego  6  11   9  0  0   0   0   0  0   0  0   0
#> 319             Los Angeles  8  23  19  2  5   4   1   0  0   2  1   0
#> 320               Tampa Bay 34 130 123 16 31  21   5   4  1   5  5   0
#> 321                 Detroit 36 155 140 20 33  23   8   1  1  12 14   0
#> 322               Milwaukee  1   1   1  0  0   0   0   0  0   0  0   0
#> 323               Cleveland 37 174 143 31 57  35  17   3  2  16 22   2
#> 324                   Miami  7  15  10  0  0   0   0   0  0   1  0   0
#> 325               St. Louis 18  32  29  2  1   1   0   0  0   0  3   0
#> 326             Los Angeles 11  38  35  2  5   2   2   0  1   5  3   0
#> 327             Los Angeles  9  26  24  2  6   6   0   0  0   1  2   0
#> 328               St. Louis  6  14  12  0  1   0   1   0  0   0  1   0
#> 329                New York 36 146 139 16 36  27   5   2  2   7  6   0
#> 330                 Chicago 17  48  44  1  9   5   3   0  1   4  4   0
#> 331                 Arizona 11  38  35  1  6   4   2   0  0   1  2   1
#> 332                 Chicago 36 142 121 12 28  16   7   0  5  14 19   0
#> 333                   Miami  4   7   5  0  2   2   0   0  0   0  0   0
#> 334       Atlanta,Baltimore  3  10   8  0  0   0   0   0  0   0  2   0
#> 335                 Oakland 35 137 128 13 40  27   9   0  4  16  8   0
#> 336              Cincinnati  9  17  17  1  3   2   1   0  0   0  0   0
#> 337                Colorado 40 169 157 28 46  37   6   1  2  17 12   1
#> 338                  Boston 15  46  39  0  6   5   1   0  0   0  3   1
#> 339                 Chicago  6  13  13  0  0   0   0   0  0   0  0   0
#> 340           San Francisco  7  14  13  0  1   0   1   0  0   0  0   0
#> 341               Milwaukee 35 129 118 11 27  19   4   0  4  17 11   2
#> 342               Cleveland  6  23  23  2  6   6   0   0  0   2  0   0
#> 343              Pittsburgh  7  19  18  0  1   1   0   0  0   0  0   0
#> 344              Washington 10  37  33  0  4   3   1   0  0   3  3   0
#> 345              Pittsburgh  7  12  10  0  1   1   0   0  0   0  0   0
#> 346               Milwaukee  9  13  11  0  1   1   0   0  0   0  0   0
#> 347              Pittsburgh  4   3   2  1  0   0   0   0  0   0  1   0
#> 348               Tampa Bay 14  54  48  1 16  14   2   0  0   5  5   2
#> 349               Tampa Bay 38 151 136 13 34  27   4   0  3  20  9   2
#> 350              Cincinnati  9  14  12  2  3   2   0   1  0   2  0   0
#> 351               Baltimore 24  63  58  3 13  10   1   0  2   7  2   0
#> 352               Milwaukee 18  76  72  5 21  17   3   0  1  10  3   0
#> 353                Colorado  4   5   5  0  0   0   0   0  0   0  0   0
#> 354               St. Louis  4   9   7  0  2   2   0   0  0   1  0   0
#> 355               St. Louis  4   8   7  3  3   3   0   0  0   1  1   0
#> 356                 Detroit  3   8   7  0  1   1   0   0  0   0  1   0
#> 357               Baltimore 40 175 163 30 51  32   9   1  9  22 12   0
#> 358           San Francisco  2   2   2  0  0   0   0   0  0   0  0   0
#> 359               Tampa Bay  6  19  16  2  2   1   0   0  1   1  2   0
#> 360               Milwaukee 21  77  69  8 14  10   2   0  2   7  8   2
#> 361               St. Louis  1   1   1  0  0   0   0   0  0   0  0   0
#> 362                 Houston 32 104  96  6 16  11   3   0  2   7  4   0
#> 363                 Atlanta 39 172 148 14 44  32  12   0  0  13 24   4
#> 364              Cincinnati  2   3   3  0  1   0   1   0  0   0  0   0
#> 365             Los Angeles  2   4   4  0  1   1   0   0  0   0  0   0
#> 366              Pittsburgh 38 163 152 22 43  28  10   0  5  24  7   0
#> 367                   Texas 34 116 106 10 27  19   5   0  3  13  6   0
#> 368                 Toronto 32 128 117 20 31  21   5   2  3  16  9   0
#> 369               St. Louis  6  16  14  1  3   3   0   0  0   0  1   0
#> 370                 Detroit 37 152 139 19 43  28   8   0  7  20  9   1
#> 371                 Detroit  7  25  23  1  2   2   0   0  0   0  2   0
#> 372                   Miami  5  19  18  1  4   3   1   0  0   3  0   0
#> 373               Minnesota 37 148 130 15 29  20   5   1  3  21 15   5
#> 374           San Francisco 27  75  72  8 14  10   1   0  3   7  2   0
#> 375               Minnesota  1   1   1  0  0   0   0   0  0   0  0   0
#> 376                New York 27  53  49  4 11   6   3   0  2   8  4   0
#> 377                 Atlanta 37 159 143 20 45  40   4   0  1  24 14   1
#> 378                New York 31 123 108 14 32  22   4   0  6  25 13   0
#> 379                 Detroit 31 117 107  7 25  15   8   1  1   9  6   0
#> 380              Pittsburgh 37 159 136 21 46  28  11   1  6  28 17   3
#> 381           San Francisco 20  46  40  1 10   9   1   0  0   4  6   0
#> 382            Philadelphia  1   2   2  0  0   0   0   0  0   0  0   0
#> 383                 Houston  1   3   3  1  1   1   0   0  0   0  0   0
#> 384                Colorado 21  62  50 11 12   5   1   3  3  10 11   0
#> 385              Pittsburgh 30 106  99 12 23  17   4   0  2   5  5   3
#> 386              Cincinnati  8  16  15  0  6   4   1   1  0   2  1   0
#> 387               San Diego 35 126 119 10 30  21   3   0  6  15  4   0
#> 388                 Seattle 37 130 113 16 26  15   5   1  5   9 16   0
#> 389                 Atlanta  8  19  15  1  1   0   1   0  0   0  0   0
#> 390               St. Louis 37 140 130 11 41  34   5   0  2   9  9   1
#> 391                New York 10  16  15  1  1   0   1   0  0   2  1   0
#> 392                 Chicago 32 112  93  9 19  12   1   0  6  12 16   2
#> 393              Washington 28  64  60  5 12   6   3   0  3  11  3   0
#> 394             Kansas City 35 129 117 11 31  18   9   0  4  22 11   0
#> 395                   Texas 33 137 129 18 39  23   9   0  7  24  8   0
#> 396                Colorado  2   8   8  1  3   2   1   0  0   0  0   0
#> 397                 Seattle 38 158 142 13 37  30   3   1  3  14 14   1
#> 398                   Miami 11  31  30  0  7   5   2   0  0   2  1   0
#> 399              Pittsburgh  5  13   9  0  1   1   0   0  0   1  1   0
#> 400              Cincinnati  2   4   4  0  1   1   0   0  0   0  0   0
#> 401               Cleveland 35 147 129 14 32  18   8   0  6  14 17   2
#> 402             Kansas City 34 139 127 15 41  29   8   1  3  17  9   0
#> 403                 Oakland 21  69  62 10 12   4   6   0  2   5  7   0
#> 404                New York  9  21  18  2  1   1   0   0  0   0  3   0
#> 405                New York 25  99  91  8 33  24   7   0  2  11  8   2
#> 406               Cleveland 33  92  84 10 31  23   7   1  0  11  5   0
#> 407                New York 12  35  34  2 10   8   1   1  0   3  0   0
#> 408               San Diego  4  19  17  2  2   2   0   0  0   0  2   0
#> 409                  Boston 36 139 120 13 29  15   6   0  8  22 18   1
#> 410                  Boston  7  21  19  2  4   3   1   0  0   1  2   0
#> 411                 Toronto  9  30  25  2  4   4   0   0  0   2  3   1
#> 412             Los Angeles 11  33  30  5  9   7   2   0  0   3  3   1
#> 413               Baltimore  3  10  10  2  2   1   0   0  1   1  0   0
#> 414              Cincinnati 21  61  55  1  9   7   2   0  0   1  2   0
#> 415               Milwaukee  6  12  11  0  2   2   0   0  0   0  1   0
#> 416                   Miami  1   3   3  0  0   0   0   0  0   0  0   0
#> 417                New York  6  12  11  1  2   1   1   0  0   0  1   0
#> 418    Los Angeles,New York 16  39  37  5  4   1   3   0  0   2  2   0
#> 419               Minnesota  1   3   3  0  0   0   0   0  0   0  0   0
#> 420               Baltimore  1   2   2  0  0   0   0   0  0   0  0   0
#> 421               San Diego 37 150 136 20 29  16   5   1  7  24 13   0
#> 422               Minnesota 17  54  50  5 13   5   6   1  1   5  2   0
#> 423                 Arizona  2   5   5  0  0   0   0   0  0   0  0   0
#> 424            Philadelphia  8  14  12  0  1   1   0   0  0   0  0   0
#> 425                 Houston  1   3   0  0  0   0   0   0  0   0  1   0
#> 426                   Texas  5  15  14  0  5   5   0   0  0   5  0   0
#> 427               Tampa Bay  1   2   2  0  0   0   0   0  0   0  0   0
#> 428             Kansas City 13  46  45  6 11   8   2   0  1   3  0   0
#> 429                  Boston 35 147 130  9 29  17   7   0  5  17 16   5
#> 430                 Arizona 33 132 126 15 32  24   6   2  0   7  2   0
#> 431                   Miami 38 155 146 11 38  31   5   0  2  17  7   0
#> 432                 Arizona 14  38  33  3  8   7   0   0  1   2  4   0
#> 433           San Francisco 36 147 136 14 30  26   3   1  0   9  9   0
#> 434           San Francisco 37 168 148 25 48  31  12   1  4  16 16   0
#> 435               Baltimore 33 132 125 14 34  28   4   0  2  15  5   0
#> 436           San Francisco  4  10  10  0  1   1   0   0  0   0  0   0
#> 437                Colorado  1   1   1  0  1   1   0   0  0   0  0   0
#> 438               Baltimore  4  19  16  5  6   3   0   0  3   4  2   0
#> 439               Milwaukee 38 139 130 15 38  28   7   0  3   9  7   0
#> 440                 Oakland  8   8   6  1  0   0   0   0  0   0  2   0
#> 441                Colorado 31  98  90 14 28  14   7   2  5  14  8   0
#> 442               Baltimore 25  78  73  9 17  11   2   0  4  13  4   0
#> 443             Los Angeles 39 168 140 17 34  19   5   1  9  15 24   0
#> 444                  Boston 37 167 154 18 49  39   6   0  4  15 11   1
#> 445            Boston,Texas  8  24  22  1  2   2   0   0  0   0  2   0
#> 446               Minnesota  1   2   2  0  2   2   0   0  0   0  0   0
#> 447              Cincinnati 28 110 100  8 28  23   5   0  0   8  9   0
#> 448           San Francisco 18  76  71 13 20  13   4   1  2  13  5   0
#> 449                 Arizona 23  59  50  4  9   8   1   0  0   4  5   0
#> 450                 Arizona 34 116 103  9 24  16   7   0  1  14 11   1
#> 451               St. Louis 37 153 137 16 44  29   9   0  6  24 14   1
#> 452               Milwaukee  3   6   3  1  0   0   0   0  0   1  1   0
#> 453             Los Angeles 24  80  76  1 18  14   3   0  1   9  2   0
#> 454                 Atlanta  3  11  10  0  3   3   0   0  0   1  0   0
#> 455       Detroit,Milwaukee 23  48  45  3  8   5   3   0  0   1  2   0
#> 456               Cleveland 19  62  48  7 10   7   1   1  1   6 13   1
#> 457             Kansas City 33 123 121  9 31  19   5   0  7  12  2   0
#> 458                 Atlanta  6  12  11  1  2   2   0   0  0   0  1   0
#> 459                 Atlanta 37 167 145 19 42  27  11   2  2  23 18   1
#> 460               Milwaukee 16  49  45  7 13  11   1   1  0   3  4   0
#> 461           San Francisco  3   4   3  0  0   0   0   0  0   0  0   0
#> 462                 Oakland 17  61  55  4 18   7   7   1  3   7  6   0
#> 463                   Miami  7  14  11  0  1   1   0   0  0   0  0   0
#> 464              Cincinnati 32 125 115 16 31  24   5   0  2  11  7   1
#> 465                 Atlanta 28 104  97 14 23  14   7   1  1  10  5   0
#> 466                 Toronto 38 147 138 22 37  29   4   0  4  17  7   0
#> 467                New York 13  33  32  3  7   5   1   0  1   1  1   0
#> 468                New York 23  73  66  4 15  12   2   0  1   7  4   1
#> 469               Minnesota 35 144 133 12 31  14  11   2  4  21  9   0
#> 470              Pittsburgh 35 130 117 18 26  20   3   1  2   9 12   2
#> 471               Minnesota  1   4   3  0  1   1   0   0  0   0  1   0
#> 472                 Arizona 38 164 152 24 47  30   9   2  6  22  9   0
#> 473           San Francisco 36 151 133 20 40  26   8   0  6  30 16   1
#> 474                   Miami 33 143 136 10 33  25   5   1  2  10  6   0
#> 475                 Detroit  2   5   5  0  0   0   0   0  0   0  0   0
#> 476             Los Angeles 13  56  52  6 17  10   5   1  1   4  4   0
#> 477             Los Angeles 37 156 143 27 42  20   7   0 15  30 10   3
#> 478                 Chicago  2   5   4  0  0   0   0   0  0   0  0   0
#> 479               Cleveland 24  59  49  5 11   6   2   1  2   6  9   2
#> 480                 Chicago 39 156 150  9 32  28   4   0  0  12  3   0
#> 481               Milwaukee 32 119 113  8 24  12   7   0  5  14  4   0
#> 482               Tampa Bay  1   2   2  0  0   0   0   0  0   0  0   0
#> 483                  Boston 38 156 146 21 40  31   5   0  4  14  8   1
#> 484               Cleveland 21  81  69 12 13   9   4   0  0   5  9   0
#> 485              Washington 29 119 113 12 25  13   6   0  6  22  4   0
#> 486                   Texas  1   2   2  0  0   0   0   0  0   0  0   0
#> 487                 Houston 27  95  85 10 21  10   8   0  3  11  9   0
#> 488                 Arizona  3   5   2  0  0   0   0   0  0   0  0   0
#> 489                   Miami 30 121 116 11 29  19   5   2  3  12  5   0
#> 490                New York 11  39  36  5  5   3   0   0  2   3  2   1
#> 491                 Oakland 39 162 150 18 40  27   6   2  5  20 12   0
#> 492                 Arizona  1   1   1  0  0   0   0   0  0   0  0   0
#> 493               Baltimore 10  31  27  3  7   4   0   1  2   5  4   0
#> 494              Washington 16  72  61  9 14   9   5   0  0   4  9   0
#> 495            Philadelphia 38 163 155 19 45  37   5   3  0   7  8   0
#> 496                 Toronto 24 113 107 15 33  23   8   0  2  15  6   0
#> 497               St. Louis 34 126 117  8 32  23   6   0  3  15  9   0
#> 498                   Miami  1   1   1  0  0   0   0   0  0   0  0   0
#> 499             Kansas City 17  65  61  4 12  10   2   0  0   1  2   0
#> 500               Tampa Bay 31 102  94  6 18  11   5   0  2  10  3   0
#> 501                 Chicago 37 166 141 17 41  17  15   1  8  25 18   2
#> 502              Washington  5  10  10  0  3   2   1   0  0   0  0   0
#> 503             Los Angeles  4   8   7  1  2   2   0   0  0   1  1   0
#> 504              Washington 24  63  57 11 17  11   4   0  2   6  6   0
#> 505               Minnesota 17  51  46  6  8   4   2   2  0   4  3   0
#> 506                 Chicago  1   1   1  0  0   0   0   0  0   0  0   0
#> 507                New York 36 152 126 24 39  27   5   0  7  21 19   1
#> 508              Pittsburgh 27  61  58  3 12   8   2   0  2   2  1   0
#> 509                   Texas  1   2   2  1  1   1   0   0  0   0  0   0
#> 510               Milwaukee 29  61  55  6 13  10   1   1  1   4  5   0
#> 511             Los Angeles 39 144 135 17 31  22   5   0  4   7  8   0
#> 512                 Detroit 15  31  29  2  5   5   0   0  0   0  1   0
#> 513           San Francisco  1   1   1  0  0   0   0   0  0   0  0   0
#> 514                   Texas 19  55  47  7 14  12   1   0  1   3  7   0
#> 515               Minnesota 31 110 104 13 30  21   5   2  2  10  5   0
#> 516                Colorado 33 103 100 11 26  20   2   1  3  11  2   2
#> 517                 Chicago 18  49  44  2  8   5   3   0  0   1  4   2
#> 518              Washington  4   9   9  0  1   1   0   0  0   0  0   0
#> 519               San Diego  7  15  14  2  2   1   0   1  0   1  0   0
#> 520                   Texas  2   8   8  1  1   1   0   0  0   0  0   0
#> 521            Philadelphia 19  41  38  2 11   8   2   0  1   4  2   0
#> 522                 Seattle 14  36  31  3  8   4   2   0  2   3  5   0
#> 523            Philadelphia 27  95  87  7 19  15   4   0  0   7  6   2
#> 524            Philadelphia 14  48  42  2 14  10   4   0  0   4  5   1
#> 525                Colorado  4   9   9  1  1   1   0   0  0   0  0   0
#> 526                 Chicago 35 136 122 13 31  20   8   0  3  13 12   0
#> 527                New York  5  11  10  1  3   2   0   1  0   1  0   0
#> 528                 Chicago  1   3   3  0  0   0   0   0  0   0  0   0
#> 529                 Arizona 12  45  39  4  6   3   2   0  1   3  4   0
#> 530                 Chicago  1   2   2  0  1   1   0   0  0   0  0   0
#> 531                 Chicago 32 106  96  9 12   9   3   0  0   6  4   0
#> 532                  Boston 33 119 115 10 28  16   7   1  4  11  2   0
#> 533               Cleveland  1   2   1  1  1   0   0   0  1   2  1   0
#> 534               Cleveland 31 143 115 12 23  11   8   0  4  17 25   1
#> 535               Minnesota 21  72  69  7 11   8   1   2  0   4  0   0
#> 536                 Houston  3   8   7  2  2   1   0   0  1   4  0   0
#> 537             Los Angeles  1   2   2  0  1   1   0   0  0   0  0   0
#> 538               Milwaukee 19  71  68  7 15  14   0   1  0   1  2   0
#> 539               Minnesota  1   3   3  0  0   0   0   0  0   0  0   0
#> 540             Los Angeles  1   3   3  0  1   1   0   0  0   0  0   0
#> 541              Washington  7  21  20  3  7   7   0   0  0   0  0   0
#> 542              Cincinnati 31  77  66  9 15   8   7   0  0   3 11   2
#> 543                 Chicago  5  18  18  5  7   5   0   1  1   4  0   0
#> 544               St. Louis  3   7   7  1  2   2   0   0  0   1  0   0
#> 545                 Seattle 38 153 141 17 41  26   7   0  8  22 11   3
#> 546               Milwaukee 24  98  96 12 26  21   2   2  1   6  2   2
#> 547                 Oakland 38 152 139 16 34  23   5   3  3   5 12   1
#> 548                  Boston  3   6   6  0  0   0   0   0  0   0  0   0
#> 549               San Diego  8  19  17  4  5   4   1   0  0   0  0   0
#> 550                 Chicago 22  56  47  4 13  11   2   0  0   2  5   0
#> 551                 Atlanta 37 154 144 19 39  32   6   0  1  13  8   1
#> 552                 Detroit  1   2   2  0  0   0   0   0  0   0  0   0
#> 553            Philadelphia 16  52  49  3 14  11   3   0  0   4  3   0
#> 554                 Seattle 35 128 114 11 29  13  11   1  4   9 12   0
#> 555                 Toronto 27  67  62  8 18  10   4   1  3  15  5   0
#> 556                   Texas  9  13  11  3  1   1   0   0  0   1  1   0
#> 557               Baltimore 28  93  83  7 21  14   6   0  1   8  9   0
#> 558               St. Louis  1   1   1  0  0   0   0   0  0   0  0   0
#> 559                 Oakland 35 128 117 13 29  26   3   0  0  14 10   0
#> 560                   Miami 18  32  32  1  4   3   1   0  0   2  0   0
#> 561                   Miami  6  18  18  1  0   0   0   0  0   1  0   0
#> 562               San Diego 34 123 114  8 23  16   7   0  0   8  8   0
#> 563                 Chicago 20  82  75  6 18  12   5   0  1   6  6   2
#> 564              Washington  1   1   1  0  1   1   0   0  0   0  0   0
#> 565                 Chicago 22  64  57  7 14   7   4   0  3   8  7   0
#> 566               Tampa Bay 35 139 123 25 28  14   5   0  9  17 14   0
#> 567              Washington 32 146 134 18 43  34   6   0  3  13 10   0
#> 568               San Diego 37 136 120 12 27  20   4   1  2  10  9   1
#> 569                 Houston 37 160 139 22 47  32   8   0  7  13 18   0
#> 570                   Miami 38 162 144 25 39  15   6   0 18  34 17   3
#> 571              Pittsburgh 14  40  36  3 10   5   5   0  0   1  3   0
#> 572                   Miami 38 172 168 19 49  40   8   1  0   5  3   1
#> 573              Washington  3   5   3  0  0   0   0   0  0   0  0   0
#> 574           San Francisco  1   1   1  0  0   0   0   0  0   0  0   0
#> 575                Colorado  8  24  23  2  4   3   1   0  0   1  1   0
#> 576                 Atlanta  3   4   4  0  0   0   0   0  0   0  0   0
#> 577              Cincinnati  8  31  27  2  5   2   2   0  1   5  2   0
#> 578                 Seattle  4  10  10  1  1   1   0   0  0   0  0   0
#> 579           San Francisco 16  48  41  5 10   8   2   0  0   2  7   0
#> 580                   Miami 35  65  59  5 18  17   0   1  0   3  6   0
#> 581               Minnesota 30 115 105  8 24  18   5   0  1   8  7   2
#> 582                  Boston 28 102  95 14 26  19   6   0  1   9  6   0
#> 583               Cleveland 25  92  82  4 14   9   3   0  2   8  8   1
#> 584                New York  8  15  13  1  4   3   0   0  1   1  0   0
#> 585                 Chicago 10  20  20  2  4   3   1   0  0   5  0   0
#> 586              Pittsburgh 22  34  32  2 10  10   0   0  0   4  1   0
#> 587                New York  1   2   2  0  0   0   0   0  0   0  0   0
#> 588                 Seattle 15  49  45  4  7   5   1   1  0   1  4   0
#> 589              Washington 35 109 100 13 21  16   2   0  3  14  8   2
#> 590                 Atlanta  6  14  12  0  1   1   0   0  0   2  0   0
#> 591                New York 35 142 119 19 33  18   7   0  8  24 19   1
#> 592                New York 28 102  91 11 26  17   8   0  1   9  9   0
#> 593                 Atlanta  4   4   4  1  1   0   0   0  1   1  0   0
#> 594                 Toronto  6  20  17  4  6   4   2   0  0   1  3   0
#> 595             Los Angeles  1   1   0  1  0   0   0   0  0   0  1   0
#> 596                 Toronto 10  29  26  6  8   4   4   0  0   1  3   0
#> 597                 Arizona 34 132 127  9 42  30   8   1  3  18  4   0
#> 598                 Toronto  7  26  25  3  5   2   3   0  0   2  1   0
#> 599              Washington  2   2   0  0  0   0   0   0  0   0  0   0
#> 600             Los Angeles 38 163 145 26 43  23  10   1  9  22 16   5
#> 601         Arizona,Seattle 34 136 129 15 27  17   4   0  6  13  7   0
#> 602                 Houston 31 113 104 16 28  14  10   0  4  14  8   0
#> 603                Colorado 34 140 130 19 39  29   5   0  5  24  9   2
#> 604             Los Angeles 36 117 105 15 35  23   8   0  4  19  9   0
#> 605              Washington 18  25  20  0  5   3   2   0  0   2  5   0
#> 606               San Diego 11  25  22  6  4   2   0   1  1   2  3   0
#> 607               San Diego 39 163 139 19 36  29   2   0  5  19 22   2
#> 608                   Miami  4   8   8  0  0   0   0   0  0   0  0   0
#> 609     Atlanta,Los Angeles 30 100  90 10 27  21   4   0  2   8  9   1
#> 610               Cleveland 11  36  33  3  8   7   0   0  1   4  3   0
#> 611            Philadelphia 37 140 122 11 29  20   7   1  1  11 14   1
#> 612                 Houston 34 137 122 20 20   8   2   0 10  19 13   0
#> 613                 Toronto 22  63  58  9 18   8   7   0  3  14  3   0
#> 614             Los Angeles 17  53  49  4 12   7   3   0  2   7  3   0
#> 615               Minnesota 14  47  47  2  9   5   3   0  1   1  0   0
#> 616               San Diego 36 138 127 16 36  27   6   1  2  12 11   0
#> 617             Kansas City  2   4   4  0  0   0   0   0  0   0  0   0
#> 618                  Boston  8  23  20  3  9   7   1   0  1   1  3   0
#> 619               St. Louis  2   2   2  0  0   0   0   0  0   0  0   0
#> 620                 Houston 22  69  62  9 19  12   5   1  1   6  5   0
#> 621           San Francisco  7  17  15  1  2   1   1   0  0   0  0   0
#> 622                 Oakland 38 147 118 17 32  19   6   1  6  26 23   3
#> 623             Kansas City  2   4   4  0  0   0   0   0  0   0  0   0
#> 624              Cincinnati 37 157 133 20 36  19   9   1  7  16 23   2
#> 625               St. Louis  5  12  10  1  1   1   0   0  0   1  0   0
#> 626                 Chicago  5   8   7  1  0   0   0   0  0   0  1   0
#> 627               Milwaukee  1   1   1  0  0   0   0   0  0   0  0   0
#> 628              Pittsburgh 30 130 122 11 27  17   8   0  2  11  7   1
#> 629                 Seattle  1   3   3  0  0   0   0   0  0   0  0   0
#> 630               San Diego  1   1   1  0  0   0   0   0  0   0  0   0
#> 631               Cleveland  9  24  24  0  4   4   0   0  0   3  0   0
#> 632                New York  1   3   3  0  0   0   0   0  0   0  0   0
#> 633                 Arizona  2   5   5  0  0   0   0   0  0   0  0   0
#> 634                 Seattle 18  40  36  2  4   4   0   0  0   2  3   0
#> 635              Washington  6  25  20  6  5   4   0   0  1   4  3   0
#> 636               Baltimore 12  39  35  7 11   4   4   1  2   8  2   0
#> 637            Philadelphia  7  12  11  0  2   1   1   0  0   1  1   0
#> 638                New York  8  22  21  3  6   2   3   0  1   3  1   0
#> 639               Tampa Bay  9  26  25  3  3   3   0   0  0   3  1   0
#> 640             Los Angeles  1   3   3  0  1   1   0   0  0   0  0   0
#> 641                 Detroit  5  16  16  3  7   6   1   0  0   3  0   0
#> 642               Baltimore  1   1   0  0  0   0   0   0  0   0  0   0
#> 643                 Atlanta  1   2   2  0  0   0   0   0  0   0  0   0
#> 644               St. Louis 38 162 147 23 36  22   8   1  5  15 11   0
#> 645                 Atlanta  7  19  17  0  4   4   0   0  0   4  0   0
#> 646                 Chicago  5   5   5  0  1   1   0   0  0   0  0   0
#> 647              Pittsburgh  3   3   3  0  1   1   0   0  0   0  0   0
#> 648               Baltimore  1   3   2  0  1   1   0   0  0   0  0   0
#> 649                   Miami 38 153 139 18 36  27   4   1  4  11 13   0
#> 650                Colorado 27  49  47  5 13  11   2   0  0   3  2   0
#> 651             Kansas City  1   3   3  0  2   2   0   0  0   3  0   0
#> 652                New York 27  69  68  8 12   8   2   0  2   6  1   0
#> 653               Baltimore 29 100  96 10 23  18   3   0  2   6  3   0
#> 654                 Atlanta 10  11  10  0  2   2   0   0  0   0  0   0
#> 655              Washington 25 101  90  9 17  11   4   0  2  14  7   0
#> 656              Washington  8  16  14  1  3   2   1   0  0   0  0   0
#> 657                 Oakland 24  94  82 18 20   9   7   1  3  16 12   0
#> 658                 Seattle 30 112 102  8 16   9   3   0  4   8  9   0
#>     uBB SO HBP SH SF GDP SB CS    BA   OBP   SLG   OPS
#> 1     7 28   3  0  0   6  0  0 0.293 0.344 0.471 0.816
#> 2     8 19   1  2  0   1  0  2 0.179 0.276 0.269 0.545
#> 3     3 16   0  0  0   0  1  0 0.200 0.245 0.320 0.565
#> 4     0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 5    11 26   0  2  0   1  3  1 0.294 0.364 0.440 0.804
#> 6     2 10   0  2  0   1  1  0 0.279 0.302 0.344 0.646
#> 7     4 16   0  2  0   1  1  0 0.190 0.261 0.238 0.499
#> 8     8  8   1  0  0   1  0  2 0.290 0.380 0.387 0.767
#> 9     3 14   1  0  2   5  6  2 0.224 0.250 0.272 0.522
#> 10    0  0   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 11    6 25   0  0  1   1  0  0 0.245 0.302 0.434 0.736
#> 12    4 14   1  1  1   4  2  0 0.244 0.290 0.349 0.639
#> 13    3  8   0  1  0   0  0  0 0.111 0.333 0.222 0.556
#> 14    0  4   0  6  0   1  0  0 0.167 0.167 0.167 0.333
#> 15    0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 16   14 25   0  3  4   5  4  2 0.241 0.303 0.328 0.632
#> 17   10  3   3  1  0   4  4  2 0.356 0.412 0.407 0.820
#> 18    0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 19    5 32   3  0  4   3  0  1 0.269 0.302 0.545 0.847
#> 20    0  1   0  0  0   1  0  0 0.091 0.091 0.091 0.182
#> 21    0 10   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 22    3 16   0  0  0   2  0  0 0.218 0.247 0.321 0.567
#> 23    1  0   0  0  0   0  0  0    NA 1.000    NA    NA
#> 24    5  8   0  2  1   7  1  0 0.269 0.310 0.333 0.643
#> 25    8 18   1  1  1   2  4  1 0.271 0.313 0.343 0.656
#> 26    4 12   0  0  0   0  0  0 0.244 0.311 0.488 0.799
#> 27    1 18   1  2  0   0  0  0 0.321 0.345 0.464 0.809
#> 28    1  1   0  0  0   0  0  0 0.200 0.333 0.200 0.533
#> 29    8 21   2  0  0   1  1  0 0.292 0.364 0.371 0.734
#> 30    3  9   0  0  1   0  0  0 0.214 0.261 0.310 0.570
#> 31    0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 32   28 17   2  0  4   4  2  0 0.282 0.419 0.548 0.967
#> 33    3 12   1  0  0   2  0  0 0.250 0.344 0.286 0.629
#> 34    7 15   0  0  2   3  0  0 0.189 0.240 0.263 0.504
#> 35    1  6   1  0  1   1  1  0 0.231 0.276 0.538 0.814
#> 36    0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 37   12 44   1  0  1   1  2  1 0.255 0.321 0.518 0.838
#> 38    9 18   1  0  1   1  0  0 0.304 0.358 0.536 0.893
#> 39    3  8   1  0  2   2  0  0 0.301 0.333 0.482 0.815
#> 40    0  3   0  0  0   0  0  0 0.200 0.200 0.400 0.600
#> 41    0  9   0  0  0   2  0  0 0.208 0.222 0.302 0.524
#> 42    2  7   0  0  0   0  0  0 0.000 0.125 0.000 0.125
#> 43    8 15   0  2  1   0  6  1 0.296 0.331 0.423 0.754
#> 44    0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 45    0  1   0  0  0   0  0  0 0.250 0.250 1.000 1.250
#> 46   11 32   7  2  1   0 13  3 0.242 0.324 0.366 0.690
#> 47    4  4   0  1  0   2  0  1 0.250 0.344 0.500 0.844
#> 48    5 18   0  0  1   1  3  1 0.348 0.405 0.500 0.905
#> 49    1  8   0  0  0   1  0  0 0.286 0.310 0.393 0.703
#> 50    0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 51    0  1   0  0  0   0  0  0 0.500 0.500 1.000 1.500
#> 52    1  8   0  0  0   1  0  0 0.143 0.163 0.167 0.329
#> 53    1 11   0  0  0   0  0  0 0.097 0.125 0.097 0.222
#> 54    4 16   1  2  1   3  2  1 0.304 0.326 0.407 0.734
#> 55    0  8   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 56    1 17   0  1  0   0  0  2 0.178 0.196 0.200 0.396
#> 57    9 25   1  0  0   2  0  0 0.215 0.298 0.366 0.664
#> 58    8 17   1  1  0   1  0  3 0.197 0.291 0.329 0.620
#> 59    9 25   0  4  1   0  3  1 0.265 0.324 0.316 0.640
#> 60    0  5   0  0  0   0  0  0 0.167 0.167 0.167 0.333
#> 61    2  4   0  0  0   0  0  0 0.000 0.154 0.000 0.154
#> 62   17 14   0  0  3   3  4  1 0.269 0.353 0.388 0.741
#> 63   11 32   1  0  2   4  5  1 0.243 0.305 0.485 0.790
#> 64    4  2   0  0  0   0  0  0 0.150 0.292 0.150 0.442
#> 65   19 30   0  0  0   3  4  4 0.281 0.383 0.469 0.851
#> 66   17 45   3  0  2   1  3  1 0.294 0.376 0.517 0.893
#> 67    0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 68    0  3   0  1  0   0  0  0 0.200 0.200 0.200 0.400
#> 69    2  5   0  0  0   1  0  0 0.286 0.375 0.500 0.875
#> 70    0 13   0  2  1   0  0  0 0.056 0.053 0.056 0.108
#> 71    9 29   3  0  1   1 13  2 0.327 0.372 0.440 0.812
#> 72    0  5   0  1  0   0  0  0 0.154 0.154 0.231 0.385
#> 73   11 24   1  0  3   4  0  0 0.248 0.303 0.328 0.631
#> 74    0  5   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 75    4 36   2  0  0   4  4  1 0.352 0.383 0.549 0.932
#> 76    2  8   0  0  0   0  0  1 0.091 0.167 0.182 0.348
#> 77    8 16   1  0  1   1  0  0 0.183 0.272 0.437 0.708
#> 78    7 27   2  0  0   4  3  1 0.203 0.277 0.331 0.607
#> 79    3  4   0  0  0   1  0  0 0.231 0.375 0.231 0.606
#> 80    5 24   2  0  2   4  0  0 0.209 0.240 0.241 0.480
#> 81   17 27   1  0  0   4  0  0 0.349 0.451 0.628 1.079
#> 82    0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000
#> 83    9 24   0  0  2   3  8  1 0.248 0.294 0.384 0.678
#> 84   11 31   0  1  2   1  3  0 0.234 0.289 0.331 0.620
#> 85    9 10   0  0  1   1  0  0 0.222 0.305 0.264 0.569
#> 86    5 19   1  0  0   7  2  0 0.155 0.221 0.225 0.446
#> 87    8 15   2  0  2   2  3  0 0.237 0.318 0.395 0.713
#> 88    7 28   0  0  2   6  1  1 0.218 0.252 0.289 0.540
#> 89    0  1   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 90    0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 91   22 35   3  0  1   1  1  0 0.262 0.386 0.381 0.767
#> 92    5 16   2  3  1   0  2  0 0.246 0.312 0.304 0.616
#> 93   23 49   3  0  3   0  1  0 0.250 0.377 0.484 0.860
#> 94    1  2   0  0  0   0  0  0 0.500 0.571 1.000 1.571
#> 95    0  6   0  0  0   0  0  0 0.071 0.071 0.071 0.143
#> 96    8 30   1  0  1   7  0  0 0.213 0.263 0.299 0.562
#> 97    3 15   0  0  0   4  1  0 0.233 0.263 0.288 0.551
#> 98    4 19   1  0  2   2  0  0 0.188 0.237 0.319 0.556
#> 99    0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000
#> 100   6 35   2  0  0   0  0  0 0.230 0.295 0.448 0.743
#> 101   5 25   3  0  1   3  2  1 0.240 0.287 0.322 0.609
#> 102   2 17   1  0  0   0  3  0 0.261 0.306 0.370 0.676
#> 103   2  3   0  0  0   0  0  0 0.083 0.214 0.167 0.381
#> 104  15 23   2  1  0   3  1  0 0.356 0.449 0.475 0.924
#> 105   9 30   0  0  2   3  3  4 0.317 0.353 0.489 0.843
#> 106   8 19   2  1  1   4  0  0 0.225 0.308 0.450 0.758
#> 107   1 21   0  0  1   0  0  1 0.173 0.181 0.309 0.489
#> 108   1  0   0  0  0   0  0  0    NA 1.000    NA    NA
#> 109  15 37   2  0  0   4  0  0 0.271 0.343 0.413 0.756
#> 110   0  7   1  0  2   0  1  0 0.279 0.283 0.395 0.678
#> 111   0  2   0  0  0   1  0  0 0.455 0.455 0.545 1.000
#> 112  10 21   2  0  0   1  3  0 0.288 0.378 0.477 0.855
#> 113   9 39   1  0  0   5  1  0 0.329 0.371 0.463 0.834
#> 114   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 115   1  4   0  3  0   0  0  0 0.143 0.200 0.143 0.343
#> 116   3 12   0  0  0   1  1  0 0.275 0.315 0.490 0.805
#> 117   2  3   0  1  0   0  0  0 0.125 0.300 0.125 0.425
#> 118   0  2   0  1  0   0  0  0 0.231 0.231 0.308 0.538
#> 119   3  3   0  0  0   1  0  0 0.214 0.353 0.286 0.639
#> 120   6 13   0  0  0   1  0  0 0.220 0.304 0.420 0.724
#> 121   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 122   1 23   1  2  0   1  0  0 0.185 0.214 0.259 0.474
#> 123   2 14   0  0  0   2  4  0 0.314 0.340 0.569 0.908
#> 124   0  0   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 125   0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 126   0  5   0  0  0   0  0  0 0.143 0.143 0.143 0.286
#> 127   7 12   0  0  2   2  0  2 0.208 0.264 0.385 0.650
#> 128   0  0   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 129   6 26   4  0  0   1  2  0 0.300 0.347 0.479 0.825
#> 130   6  6   0  0  0   0  1  0 0.065 0.216 0.097 0.313
#> 131   0  4   2  0  0   1  0  0 0.207 0.258 0.207 0.465
#> 132  15 45   0  0  0   0  0  2 0.287 0.366 0.419 0.785
#> 133   1  5   0  1  0   0  0  0 0.192 0.222 0.192 0.415
#> 134   8 33   1  0  0   8  0  0 0.271 0.317 0.391 0.708
#> 135   0  2   0  2  0   0  0  0 0.250 0.250 0.250 0.500
#> 136   3 14   2  0  0   0  2  1 0.239 0.299 0.296 0.594
#> 137   2  7   0  0  0   0  0  0 0.267 0.313 0.533 0.846
#> 138  22 43   0  0  2   1  0  0 0.197 0.304 0.401 0.706
#> 139   1  2   0  0  0   0  0  0 0.154 0.214 0.231 0.445
#> 140   6 11   0  0  0   3  0  0 0.224 0.309 0.510 0.819
#> 141   4 16   0  0  1   2  6  2 0.288 0.318 0.475 0.793
#> 142   4 17   1  0  0   0  1  0 0.250 0.320 0.382 0.702
#> 143   1 11   0  0  0   0  0  0 0.219 0.242 0.469 0.711
#> 144   0  2   0  1  0   0  0  0 0.071 0.071 0.071 0.143
#> 145   0  9   0  2  0   0  0  0 0.105 0.105 0.105 0.211
#> 146   0  3   0  3  0   3  0  0 0.211 0.211 0.211 0.421
#> 147   6 13   1  0  1   0  1  2 0.300 0.354 0.467 0.820
#> 148   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 149   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 150   1  7   1  1  0   3  0  0 0.297 0.333 0.432 0.766
#> 151   1  7   0  2  0   0  1  2 0.321 0.345 0.453 0.798
#> 152   0  7   0  1  0   0  0  0 0.214 0.214 0.214 0.429
#> 153  14 29   0  3  0   0  6  2 0.287 0.369 0.417 0.787
#> 154   5 48   1  3  0   1  1  1 0.215 0.247 0.326 0.573
#> 155   0  5   0  1  0   0  0  0 0.125 0.125 0.250 0.375
#> 156   0 11   0  0  0   0  0  0 0.290 0.290 0.419 0.710
#> 157   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 158   2  5   2  0  0   0  0  0 0.316 0.435 0.895 1.330
#> 159   0  0   0  0  0   0  0  0 0.200 0.200 0.200 0.400
#> 160   0  6   0  0  0   0  0  0 0.286 0.286 0.714 1.000
#> 161  13 34   2  1  3   2  1  0 0.290 0.347 0.548 0.895
#> 162   0  3   0  0  0   1  0  0 0.200 0.200 0.200 0.400
#> 163  15 31   3  0  2   0  3  2 0.271 0.350 0.600 0.950
#> 164   7 18   0  0  1   2  0  0 0.180 0.223 0.344 0.567
#> 165  17 38   8  0  1   5  0  0 0.273 0.394 0.538 0.932
#> 166   7 23   4  0  0   2  2  0 0.297 0.353 0.469 0.821
#> 167   0  4   1  1  0   0  1  1 0.323 0.344 0.516 0.860
#> 168   0  0   0  0  1   1  0  0 0.000 0.000 0.000 0.000
#> 169  12 28   4  0  0   0  4  2 0.285 0.353 0.417 0.771
#> 170   7  6   0  2  0   1  0  0 0.189 0.318 0.243 0.561
#> 171   6  5   0  0  0   0  3  1 0.222 0.364 0.259 0.623
#> 172   4 10   0  1  2   1  1  1 0.271 0.303 0.314 0.617
#> 173  19 28   0  0  3   6  0  0 0.241 0.341 0.500 0.841
#> 174   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 175   4 17   4  1  2   0  3  1 0.257 0.292 0.347 0.639
#> 176   4 25   0  1  1   1  1  2 0.240 0.267 0.360 0.627
#> 177   8 22   4  0  1   2  1  1 0.346 0.396 0.419 0.815
#> 178  10 30   2  0  0   2  1  2 0.275 0.356 0.450 0.806
#> 179   0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 180  10 16   0  0  0   5  1  0 0.267 0.323 0.450 0.773
#> 181   1 13   0  0  0   0  1  1 0.107 0.138 0.286 0.424
#> 182   2 16   1  1  0   0  1  0 0.195 0.250 0.366 0.616
#> 183   7 20   5  0  2   6  0  0 0.336 0.405 0.566 0.971
#> 184   0  2   0  0  0   0  0  0 0.333 0.333 0.667 1.000
#> 185   0  4   0  0  0   0  0  0 0.100 0.100 0.100 0.200
#> 186   0  0   0  2  0   0  0  0 1.000 1.000 1.000 2.000
#> 187   6 28   2  1  0   1  0  0 0.225 0.295 0.325 0.620
#> 188   0  0   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 189   0  4   0  0  0   0  0  0 0.269 0.269 0.308 0.577
#> 190   4 12   1  1  2   4  0  0 0.252 0.273 0.435 0.708
#> 191   6 28   2  0  0   2  0  0 0.229 0.297 0.386 0.682
#> 192   0 13   0  1  0   0  0  0 0.059 0.059 0.059 0.118
#> 193  12 27   6  0  2   5  4  0 0.275 0.354 0.428 0.782
#> 194  18 35   1  1  2   2  3  3 0.216 0.302 0.358 0.660
#> 195   7 20   1  0  0   2  1  0 0.282 0.329 0.511 0.840
#> 196   2 11   0  0  1   5  0  1 0.299 0.313 0.442 0.754
#> 197   6 21   0  0  0   1  1  0 0.145 0.213 0.261 0.474
#> 198   8 28   2  0  0   1  4  2 0.331 0.373 0.682 1.056
#> 199  13 37   2  0  1   3  2  0 0.281 0.361 0.482 0.843
#> 200  10 30   1  0  2   1  0  1 0.250 0.307 0.444 0.750
#> 201   1  5   1  1  0   0  0  0 0.091 0.231 0.091 0.322
#> 202   8 14   1  0  0   2  2  2 0.203 0.309 0.254 0.563
#> 203   6 28   0  0  0   0  2  0 0.220 0.313 0.492 0.805
#> 204   6 29   1  0  0   4  3  0 0.195 0.237 0.234 0.471
#> 205   1  1   0  0  0   0  0  0 0.000 0.500 0.000 0.500
#> 206   8 36   3  0  0   1  1  1 0.231 0.297 0.376 0.673
#> 207   0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000
#> 208   0  4   0  0  0   0  0  0 0.091 0.091 0.091 0.182
#> 209  13 38   1  1  1   3  6  2 0.259 0.321 0.456 0.777
#> 210   0  6   0  1  0   0  0  0 0.182 0.182 0.182 0.364
#> 211   5 31   1  0  1   2  0  0 0.270 0.303 0.489 0.792
#> 212   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 213   1 11   0  0  0   4  0  0 0.192 0.208 0.346 0.554
#> 214   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 215  11 20   0  2  3   3  1  1 0.275 0.328 0.383 0.712
#> 216   0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 217   3 15   0  0  0   0  0  0 0.247 0.286 0.356 0.642
#> 218   8 21   0  3  2   3  0  0 0.232 0.279 0.348 0.627
#> 219  21 27   0  0  0   4  5  2 0.361 0.494 0.662 1.156
#> 220   6 28   1  0  0   1  1  0 0.212 0.288 0.258 0.545
#> 221   2 22   0  0  3   1  0  0 0.219 0.231 0.329 0.560
#> 222   6 34   3  0  0   0  4  3 0.291 0.338 0.465 0.803
#> 223   2 20   1  0  0   1  0  0 0.190 0.227 0.349 0.576
#> 224  13 26   2  0  1   6  0  1 0.225 0.311 0.338 0.649
#> 225  14 28   0  1  2   2  2  0 0.266 0.333 0.475 0.808
#> 226   0  5   2  1  0   0  0  0 0.000 0.222 0.000 0.222
#> 227   3 19   0  2  0   2  2  0 0.211 0.237 0.356 0.592
#> 228   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 229   1  1   0  0  0   0  0  0 0.000 0.250 0.000 0.250
#> 230  16 23   3  0  3   2  1  1 0.257 0.368 0.450 0.818
#> 231   8 30   0  1  0   3  5  3 0.248 0.291 0.316 0.607
#> 232   5  9   0  0  1   0  1  0 0.188 0.259 0.313 0.572
#> 233   0  0   0  0  0   0  1  0 0.467 0.467 0.600 1.067
#> 234  17 20   0  0  0   2  0  0 0.233 0.355 0.389 0.744
#> 235  13 45   3  0  1   1  2  0 0.243 0.319 0.419 0.738
#> 236   0  4   0  0  0   0  0  0 0.214 0.267 0.214 0.481
#> 237   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 238   7 20   2  0  1   0  1  0 0.256 0.307 0.410 0.717
#> 239   1  1   0  2  0   0  1  0 0.231 0.286 0.231 0.516
#> 240   3 35   0  0  1   3  2  0 0.303 0.325 0.596 0.921
#> 241   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 242   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 243   2 26   0  0  1   3  0  0 0.250 0.264 0.476 0.741
#> 244   0  0   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 245   2 13   5  0  0   3  4  1 0.266 0.326 0.316 0.642
#> 246   5 19   0  0  0   2  0  0 0.229 0.315 0.313 0.627
#> 247   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 248   0  5   0  2  0   0  0  0 0.000 0.000 0.000 0.000
#> 249   0 10   0  1  0   1  0  0 0.059 0.059 0.059 0.118
#> 250   5 20   0  5  1   0 15  3 0.235 0.264 0.287 0.551
#> 251   4  7   0  0  0   0  0  0 0.273 0.385 0.636 1.021
#> 252   0  9   0  0  0   0  0  0 0.188 0.188 0.188 0.375
#> 253   0  2   0  0  0   0  0  0 0.250 0.250 0.250 0.500
#> 254   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 255   1  0   0  0  0   1  0  0 0.000 0.500 0.000 0.500
#> 256   0  4   0  2  0   1  0  0 0.231 0.231 0.308 0.538
#> 257   3 29   0  1  1   2  0  0 0.248 0.263 0.318 0.581
#> 258   1  5   0  4  0   0  0  0 0.100 0.182 0.100 0.282
#> 259  25 17   2  0  0   5  3  1 0.397 0.517 0.802 1.319
#> 260   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 261   7 20   3  1  2   0  7  3 0.354 0.391 0.465 0.856
#> 262   0 14   1  0  0   1  0  0 0.231 0.259 0.231 0.490
#> 263   0  5   0  0  0   1  0  0 0.133 0.133 0.200 0.333
#> 264   2  0   0  0  0   0  0  0 0.143 0.333 0.571 0.905
#> 265  10 30   2  0  3   2  0  1 0.273 0.323 0.371 0.693
#> 266   0  3   0  0  0   0  0  0 0.353 0.353 0.588 0.941
#> 267   6 19   0  0  1   3  1  1 0.260 0.301 0.346 0.648
#> 268   0 10   0  1  0   0  0  0 0.107 0.107 0.250 0.357
#> 269   5  8   0  0  0   1  0  0 0.167 0.310 0.208 0.519
#> 270   0  3   0  2  1   0  0  0 0.300 0.273 0.300 0.573
#> 271   1  4   0  1  0   0  0  0 0.063 0.118 0.125 0.243
#> 272   1  0   0  0  0   0  0  0    NA 1.000    NA    NA
#> 273   6 15   1  0  0   2  2  1 0.229 0.299 0.357 0.656
#> 274   4 11   1  0  1   1  0  0 0.254 0.308 0.407 0.714
#> 275   6 14   1  0  0   1  2  0 0.190 0.306 0.286 0.592
#> 276   3 10   0  0  2   0  1  0 0.231 0.263 0.442 0.705
#> 277   0  1   0  1  0   0  0  0 0.238 0.238 0.238 0.476
#> 278   4 29   1  2  1   2  2  1 0.224 0.260 0.367 0.627
#> 279   2  7   1  1  0   0  0  0 0.200 0.286 0.400 0.686
#> 280   0  4   0  1  0   1  0  0 0.333 0.333 0.400 0.733
#> 281   5 23   0  0  2   3  4  1 0.291 0.320 0.453 0.773
#> 282   6 17   0  0  0   3  6  0 0.247 0.293 0.301 0.594
#> 283   7 16   1  0  1   4  2  1 0.213 0.281 0.288 0.568
#> 284   0  3   0  0  0   0  0  0 0.258 0.258 0.452 0.710
#> 285  11 15   3  0  2   4  0  0 0.241 0.347 0.342 0.689
#> 286  18 27   1  0  0   2  3  1 0.306 0.409 0.463 0.872
#> 287  10 25   0  0  1   6  1  1 0.272 0.338 0.392 0.730
#> 288   5 43   1  0  1   5  0  0 0.232 0.262 0.457 0.719
#> 289   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 290   0  3   0  0  0   0  0  0 0.200 0.200 0.267 0.467
#> 291   5 30   1  0  0   1  1  2 0.245 0.286 0.368 0.654
#> 292  12 19   1  0  1   2  0  0 0.227 0.306 0.328 0.634
#> 293   9 18   0  0  0   6  0  0 0.261 0.346 0.435 0.781
#> 294   5 12   2  1  0   2  4  3 0.303 0.350 0.321 0.672
#> 295   0  4   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 296   5 19   2  1  3   2  5  4 0.273 0.302 0.353 0.655
#> 297   1 20   0  0  2   1  0  1 0.213 0.216 0.270 0.486
#> 298   6 22   1  0  0   0  2  3 0.274 0.324 0.411 0.734
#> 299   2 12   1  1  1   2  0  0 0.200 0.275 0.311 0.586
#> 300   0  2   0  0  0   0  0  0 0.250 0.250 0.250 0.500
#> 301   1 14   0  0  0   1  0  0 0.147 0.171 0.235 0.407
#> 302   2 10   0  0  0   1  0  1 0.279 0.311 0.326 0.637
#> 303   1  2   0  0  0   0  1  0 0.250 0.400 0.250 0.650
#> 304   6 24   2  0  0   5  3  0 0.254 0.304 0.420 0.724
#> 305   5 15   0  0  0   0  0  0 0.292 0.358 0.479 0.838
#> 306   2  2   0  0  0   0  1  0 0.000 0.333 0.000 0.333
#> 307   0  1   0  0  0   0  0  0 0.400 0.400 0.400 0.800
#> 308   4 19   1  1  1   3  0  0 0.179 0.231 0.298 0.528
#> 309  16 25   2  1  0   0  0  2 0.224 0.345 0.408 0.753
#> 310   1  0   0  0  0   0  0  0 0.200 0.333 0.200 0.533
#> 311  10 28   6  0  0   2  3  0 0.263 0.351 0.390 0.741
#> 312   2  2   0  0  0   1  0  1 0.273 0.385 0.455 0.839
#> 313   0  1   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 314   0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 315   8 43   1  0  0   6  2  0 0.223 0.268 0.324 0.592
#> 316   9 29   0  0  0   6  0  0 0.290 0.331 0.386 0.717
#> 317   0  4   0  0  0   1  0  0 0.067 0.067 0.067 0.133
#> 318   0  7   0  2  0   0  0  0 0.000 0.000 0.000 0.000
#> 319   1  4   0  3  0   1  0  0 0.263 0.300 0.316 0.616
#> 320   5 23   2  0  0   2  7  0 0.252 0.292 0.382 0.674
#> 321  14 24   0  0  1   2  3  1 0.236 0.303 0.329 0.632
#> 322   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 323  20 23   5  2  2   0  6  3 0.399 0.488 0.601 1.090
#> 324   0  6   0  5  0   0  0  0 0.000 0.000 0.000 0.000
#> 325   3 10   0  0  0   0  0  0 0.034 0.125 0.034 0.159
#> 326   3 11   0  0  0   0  0  0 0.143 0.211 0.286 0.496
#> 327   2 11   0  0  0   2  0  0 0.250 0.308 0.250 0.558
#> 328   1  5   0  1  0   0  0  0 0.083 0.154 0.167 0.321
#> 329   6 29   1  0  0   4  4  2 0.259 0.295 0.367 0.661
#> 330   4 16   0  0  0   1  3  0 0.205 0.271 0.341 0.612
#> 331   1 12   1  0  0   0  1  0 0.171 0.237 0.229 0.465
#> 332  19 39   1  0  1   3  0  0 0.231 0.338 0.413 0.751
#> 333   0  2   0  2  0   0  0  0 0.400 0.400 0.400 0.800
#> 334   2  2   0  0  0   0  0  0 0.000 0.200 0.000 0.200
#> 335   8 31   1  0  0   1  2  0 0.313 0.358 0.477 0.834
#> 336   0 10   0  0  0   0  0  0 0.176 0.176 0.235 0.412
#> 337  11 33   0  0  0   5  7  1 0.293 0.343 0.382 0.725
#> 338   2  8   1  3  0   2  0  0 0.154 0.233 0.179 0.412
#> 339   0  6   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 340   0  7   0  1  0   1  0  0 0.077 0.077 0.154 0.231
#> 341   9 31   0  0  0   3  0  0 0.229 0.295 0.364 0.659
#> 342   0  6   0  0  0   1  1  0 0.261 0.261 0.261 0.522
#> 343   0  9   0  1  0   0  0  0 0.056 0.056 0.056 0.111
#> 344   3  7   0  1  0   0  0  0 0.121 0.194 0.152 0.346
#> 345   0  6   0  2  0   0  0  0 0.100 0.100 0.100 0.200
#> 346   0  4   0  2  0   1  0  0 0.091 0.091 0.091 0.182
#> 347   1  1   0  0  0   0  0  0 0.000 0.333 0.000 0.333
#> 348   3  1   0  0  1   0  1  1 0.333 0.389 0.375 0.764
#> 349   7 34   2  0  4   0  1  1 0.250 0.298 0.346 0.644
#> 350   0  3   0  2  0   0  0  0 0.250 0.250 0.417 0.667
#> 351   2 12   1  2  0   0  1  4 0.224 0.262 0.345 0.607
#> 352   3  7   0  0  1   4  0  0 0.292 0.316 0.375 0.691
#> 353   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 354   0  3   0  2  0   0  0  0 0.286 0.286 0.286 0.571
#> 355   1  2   0  0  0   0  0  0 0.429 0.500 0.429 0.929
#> 356   1  1   0  0  0   1  0  0 0.143 0.250 0.143 0.393
#> 357  12 29   0  0  0   4  6  0 0.313 0.360 0.546 0.906
#> 358   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 359   2  6   0  1  0   0  0  1 0.125 0.222 0.313 0.535
#> 360   6 16   0  0  0   2  0  0 0.203 0.286 0.319 0.605
#> 361   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 362   4 37   1  1  2   0  2  2 0.167 0.204 0.260 0.464
#> 363  20 19   0  0  0   5  0  0 0.297 0.395 0.378 0.774
#> 364   0  2   0  0  0   0  0  0 0.333 0.333 0.667 1.000
#> 365   0  0   0  0  0   0  0  0 0.250 0.250 0.250 0.500
#> 366   7 34   3  1  0   4  8  1 0.283 0.327 0.447 0.775
#> 367   6 23   2  2  0   2  6  3 0.255 0.307 0.387 0.694
#> 368   9 23   2  0  0   5  1  1 0.265 0.328 0.419 0.747
#> 369   1  9   0  1  0   0  0  0 0.214 0.267 0.214 0.481
#> 370   8 32   3  0  1   4  1  0 0.309 0.362 0.518 0.880
#> 371   2  3   0  0  0   1  0  0 0.087 0.160 0.087 0.247
#> 372   0  4   0  0  1   0  0  0 0.222 0.211 0.278 0.488
#> 373  10 23   1  0  2   7  1  1 0.223 0.304 0.346 0.650
#> 374   2 20   1  0  0   4  0  1 0.194 0.227 0.333 0.560
#> 375   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 376   4 12   0  0  0   1  0  0 0.224 0.283 0.408 0.691
#> 377  13 23   1  0  1   6  9  1 0.315 0.377 0.364 0.741
#> 378  13 25   0  0  2   3  0  0 0.296 0.366 0.500 0.866
#> 379   6 28   1  2  1   6  0  0 0.234 0.278 0.355 0.633
#> 380  14 26   3  0  3   3  2  1 0.338 0.415 0.566 0.981
#> 381   6  9   0  0  0   3  0  0 0.250 0.348 0.275 0.623
#> 382   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 383   0  2   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 384  11 16   0  0  1   0  0  0 0.240 0.371 0.560 0.931
#> 385   2 19   0  2  0   3  2  0 0.232 0.269 0.333 0.603
#> 386   1  1   0  0  0   0  0  0 0.400 0.438 0.600 1.038
#> 387   4 30   0  0  3   3  0  1 0.252 0.270 0.429 0.698
#> 388  16 27   1  0  0   2  3  0 0.230 0.331 0.425 0.756
#> 389   0  8   0  4  0   0  0  0 0.067 0.067 0.133 0.200
#> 390   8 17   0  0  1   3  0  0 0.315 0.357 0.400 0.757
#> 391   1  4   0  0  0   3  0  0 0.067 0.125 0.133 0.258
#> 392  14 25   3  0  0   3  0  0 0.204 0.339 0.409 0.748
#> 393   3 14   0  0  1   0  0  0 0.200 0.234 0.400 0.634
#> 394  11 17   0  0  1   4  0  0 0.265 0.326 0.444 0.770
#> 395   8 34   0  0  0   5  1  0 0.302 0.343 0.535 0.878
#> 396   0  2   0  0  0   0  0  0 0.375 0.375 0.500 0.875
#> 397  13 30   2  0  0   1  4  1 0.261 0.335 0.359 0.695
#> 398   1  8   0  0  0   2  0  0 0.233 0.258 0.300 0.558
#> 399   1  3   0  3  0   0  0  0 0.111 0.200 0.111 0.311
#> 400   0  2   0  0  0   0  0  0 0.250 0.250 0.250 0.500
#> 401  15 43   1  0  0   2  0  0 0.248 0.340 0.450 0.790
#> 402   9 18   1  0  2   3  0  0 0.323 0.367 0.472 0.839
#> 403   7 18   0  0  0   0  0  0 0.194 0.275 0.387 0.662
#> 404   3  7   0  0  0   1  0  0 0.056 0.190 0.056 0.246
#> 405   6  8   0  0  0   5  1  1 0.363 0.414 0.505 0.920
#> 406   5 13   1  1  1   1  0  1 0.369 0.407 0.476 0.883
#> 407   0  8   0  1  0   1  0  0 0.294 0.294 0.382 0.676
#> 408   2  5   0  0  0   0  0  0 0.118 0.211 0.118 0.328
#> 409  17 39   0  0  1   5  0  1 0.242 0.338 0.492 0.830
#> 410   2  6   0  0  0   1  0  0 0.211 0.286 0.263 0.549
#> 411   2  6   0  0  2   0  0  0 0.160 0.233 0.160 0.393
#> 412   2  3   0  0  0   2  0  0 0.300 0.364 0.367 0.730
#> 413   0  0   0  0  0   0  0  0 0.200 0.200 0.500 0.700
#> 414   2 15   2  2  0   0  1  0 0.164 0.220 0.200 0.420
#> 415   1  6   0  0  0   0  0  0 0.182 0.250 0.182 0.432
#> 416   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 417   1  5   0  0  0   0  0  0 0.182 0.250 0.273 0.523
#> 418   2 17   0  0  0   0  1  1 0.108 0.154 0.189 0.343
#> 419   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 420   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 421  13 40   1  0  0   2  0  0 0.213 0.287 0.419 0.706
#> 422   2  8   0  1  1   1  1  2 0.260 0.283 0.480 0.763
#> 423   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 424   0  5   0  2  0   0  0  0 0.083 0.083 0.083 0.167
#> 425   1  0   0  2  0   0  0  0    NA 1.000    NA    NA
#> 426   0  1   1  0  0   0  2  1 0.357 0.400 0.357 0.757
#> 427   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 428   0  8   1  0  0   0  0  0 0.244 0.261 0.356 0.616
#> 429  11 19   0  0  1   5  0  0 0.223 0.306 0.392 0.698
#> 430   2 32   0  2  2   2  3  1 0.254 0.262 0.333 0.595
#> 431   7 25   2  0  0   2  0  0 0.260 0.303 0.336 0.639
#> 432   4  5   0  0  1   3  1  0 0.242 0.316 0.333 0.649
#> 433   9 22   0  0  2   5  1  1 0.221 0.265 0.257 0.523
#> 434  16 21   3  1  0   3  1  0 0.324 0.401 0.500 0.901
#> 435   5 37   0  0  2   4  2  0 0.272 0.295 0.352 0.647
#> 436   0  6   0  0  0   0  0  0 0.100 0.100 0.100 0.200
#> 437   0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000
#> 438   2  2   1  0  0   0  0  0 0.375 0.474 0.938 1.411
#> 439   7 24   1  0  1   2  4  0 0.292 0.331 0.415 0.746
#> 440   2  5   0  0  0   0  0  0 0.000 0.250 0.000 0.250
#> 441   8 24   0  0  0   0  0  0 0.311 0.367 0.600 0.967
#> 442   4 18   1  0  0   3  0  1 0.233 0.282 0.425 0.707
#> 443  24 42   4  0  0   1  0  2 0.243 0.369 0.486 0.855
#> 444  10 19   1  0  1   2  0  0 0.318 0.365 0.435 0.800
#> 445   2 11   0  0  0   0  1  0 0.091 0.167 0.091 0.258
#> 446   0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000
#> 447   9  6   0  1  0   3  0  0 0.280 0.339 0.330 0.669
#> 448   5 14   0  0  0   0  1  0 0.282 0.329 0.451 0.780
#> 449   5  7   0  3  1   3  0  0 0.180 0.250 0.200 0.450
#> 450  10 25   0  0  2   1  2  1 0.233 0.302 0.330 0.632
#> 451  13 28   1  0  1   5  0  0 0.321 0.386 0.518 0.904
#> 452   1  2   0  2  0   0  0  0 0.000 0.250 0.000 0.250
#> 453   2 16   0  1  1   3  0  0 0.237 0.253 0.316 0.569
#> 454   0  1   0  1  0   0  0  0 0.300 0.300 0.300 0.600
#> 455   2 13   0  1  0   2  0  0 0.178 0.213 0.244 0.457
#> 456  12 18   0  1  0   0  0  0 0.208 0.377 0.333 0.710
#> 457   2 19   0  0  0   4  0  0 0.256 0.268 0.471 0.739
#> 458   1  6   0  0  0   0  0  0 0.182 0.250 0.182 0.432
#> 459  17 26   1  2  1   0  6  6 0.290 0.370 0.434 0.804
#> 460   4 15   0  0  0   0  0  1 0.289 0.347 0.356 0.702
#> 461   0  2   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 462   6  8   0  0  0   2  0  0 0.327 0.393 0.655 1.048
#> 463   0  2   0  3  0   0  0  0 0.091 0.091 0.091 0.182
#> 464   6 17   1  1  1   2  4  1 0.270 0.315 0.365 0.680
#> 465   5 15   2  0  0   3  0  0 0.237 0.288 0.361 0.649
#> 466   7 27   0  1  1   2  5  2 0.268 0.301 0.384 0.685
#> 467   1  7   0  0  0   1  0  0 0.219 0.242 0.344 0.586
#> 468   3 20   2  0  1   2  0  0 0.227 0.288 0.303 0.591
#> 469   9 29   0  0  2   9  0  0 0.233 0.278 0.436 0.714
#> 470  10 22   0  0  1   1  8  2 0.222 0.292 0.316 0.609
#> 471   1  0   0  0  0   0  0  0 0.333 0.500 0.333 0.833
#> 472   9 29   0  0  3   3  8  2 0.309 0.341 0.513 0.855
#> 473  15 11   1  0  1   4  1  0 0.301 0.377 0.496 0.874
#> 474   6 16   1  0  0   2  0  0 0.243 0.280 0.338 0.618
#> 475   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 476   4 12   0  0  0   0  0  0 0.327 0.375 0.519 0.894
#> 477   7 13   2  0  1   4  1  0 0.294 0.346 0.657 1.003
#> 478   0  2   0  1  0   0  0  0 0.000 0.000 0.000 0.000
#> 479   7  9   1  0  0   2  0  0 0.224 0.356 0.429 0.785
#> 480   3 17   0  0  3   5  7  1 0.213 0.224 0.240 0.464
#> 481   4 16   1  0  1   6  0  0 0.212 0.244 0.407 0.651
#> 482   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 483   7 21   1  0  1   6  2  2 0.274 0.314 0.390 0.705
#> 484   9  8   1  1  1   2  5  0 0.188 0.288 0.246 0.534
#> 485   4 24   0  0  2   5  0  0 0.221 0.244 0.434 0.677
#> 486   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 487   9 30   0  0  1   1  0  0 0.247 0.316 0.447 0.763
#> 488   0  2   0  3  0   0  0  0 0.000 0.000 0.000 0.000
#> 489   5 14   0  0  0   1  2  0 0.250 0.281 0.405 0.686
#> 490   1 15   1  0  0   0  1  0 0.139 0.205 0.306 0.511
#> 491  12 16   0  0  0   2  1  0 0.267 0.321 0.433 0.754
#> 492   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 493   4  9   0  0  0   0  0  0 0.259 0.355 0.556 0.910
#> 494   9 12   1  0  1   5  0  0 0.230 0.333 0.311 0.645
#> 495   8 16   0  0  0   1 11  2 0.290 0.325 0.361 0.686
#> 496   6 15   0  0  0   1  6  1 0.308 0.345 0.439 0.784
#> 497   9 35   0  0  0   3  1  1 0.274 0.325 0.402 0.727
#> 498   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 499   2 12   1  0  1   0  1  0 0.197 0.231 0.230 0.460
#> 500   3 22   1  3  1   0  0  0 0.191 0.222 0.309 0.531
#> 501  16 20   6  0  1   2  3  3 0.291 0.392 0.582 0.973
#> 502   0  5   0  0  0   0  0  0 0.300 0.300 0.400 0.700
#> 503   1  1   0  0  0   1  0  0 0.286 0.375 0.286 0.661
#> 504   6  5   0  0  0   0  0  0 0.298 0.365 0.474 0.839
#> 505   3  7   0  1  1   3  0  0 0.174 0.220 0.304 0.524
#> 506   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 507  18 29   5  0  2   1  1  0 0.310 0.414 0.516 0.930
#> 508   1 15   1  1  0   4  1  0 0.207 0.233 0.345 0.578
#> 509   0  0   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 510   5 12   1  0  0   1  0  0 0.236 0.311 0.345 0.657
#> 511   8 20   0  1  0   7  3  0 0.230 0.273 0.356 0.628
#> 512   1  9   0  1  0   0  1  2 0.172 0.200 0.172 0.372
#> 513   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 514   7 10   1  0  0   1  2  2 0.298 0.400 0.383 0.783
#> 515   5 29   0  1  0   1  4  1 0.288 0.321 0.433 0.754
#> 516   0 24   1  0  0   2  1  0 0.260 0.282 0.390 0.672
#> 517   2 19   0  1  0   0  0  0 0.182 0.250 0.250 0.500
#> 518   0  3   0  0  0   0  0  0 0.111 0.111 0.111 0.222
#> 519   0  7   0  1  0   0  0  0 0.143 0.143 0.286 0.429
#> 520   0  3   0  0  0   0  0  0 0.125 0.125 0.125 0.250
#> 521   2  9   0  0  1   2  0  0 0.289 0.317 0.421 0.738
#> 522   5 14   0  0  0   0  0  2 0.258 0.361 0.516 0.877
#> 523   4 12   1  1  0   6  0  1 0.218 0.277 0.264 0.541
#> 524   4 16   0  0  1   0  0  0 0.333 0.396 0.429 0.824
#> 525   0  0   0  0  0   0  0  0 0.111 0.111 0.111 0.222
#> 526  12 36   2  0  0   1  1  2 0.254 0.331 0.393 0.724
#> 527   0  1   1  0  0   0  0  0 0.300 0.364 0.500 0.864
#> 528   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 529   4 16   1  0  1   0  0  0 0.154 0.244 0.282 0.526
#> 530   0  1   0  0  0   0  0  0 0.500 0.500 0.500 1.000
#> 531   4 25   3  2  1   1  0  1 0.125 0.183 0.156 0.339
#> 532   2 14   2  0  0   5  0  0 0.243 0.269 0.426 0.695
#> 533   1  0   0  0  0   0  0  0 1.000 1.000 4.000 5.000
#> 534  24 27   1  0  2   6  1  0 0.200 0.343 0.374 0.717
#> 535   0 18   1  2  0   3  2  1 0.159 0.171 0.232 0.403
#> 536   0  3   1  0  0   0  1  0 0.286 0.375 0.714 1.089
#> 537   0  0   0  0  0   1  0  0 0.500 0.500 0.500 1.000
#> 538   2 14   0  1  0   1  0  0 0.221 0.243 0.250 0.493
#> 539   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 540   0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 541   0  6   0  1  0   0  0  0 0.350 0.350 0.350 0.700
#> 542   9  9   0  0  0   1  0  2 0.227 0.338 0.333 0.671
#> 543   0  6   0  0  0   1  0  0 0.389 0.389 0.667 1.056
#> 544   0  2   0  0  0   0  0  0 0.286 0.286 0.286 0.571
#> 545   8 25   0  0  1   2  0  0 0.291 0.340 0.511 0.851
#> 546   0 18   0  0  0   2  3  0 0.271 0.286 0.365 0.650
#> 547  11 34   0  0  1   6  1  2 0.245 0.303 0.388 0.691
#> 548   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 549   0  4   0  2  0   0  0  0 0.294 0.294 0.353 0.647
#> 550   5  4   1  2  1   1  4  2 0.277 0.352 0.319 0.671
#> 551   7 12   2  0  0   3  1  0 0.271 0.318 0.333 0.652
#> 552   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 553   3 14   0  0  0   1  0  0 0.286 0.327 0.347 0.674
#> 554  12 35   2  0  0   2  0  0 0.254 0.336 0.474 0.810
#> 555   5 19   0  0  0   4  0  0 0.290 0.343 0.532 0.876
#> 556   1  7   0  0  1   0  0  0 0.091 0.154 0.091 0.245
#> 557   9 19   1  0  0   1  0  0 0.253 0.333 0.361 0.695
#> 558   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 559  10 18   1  0  0   2  3  0 0.248 0.313 0.274 0.586
#> 560   0  6   0  0  0   3  0  0 0.125 0.125 0.156 0.281
#> 561   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 562   8 15   1  0  0   1  0  0 0.202 0.260 0.263 0.523
#> 563   4 24   1  0  0   3  0  1 0.240 0.305 0.347 0.652
#> 564   0  0   0  0  0   0  0  0 1.000 1.000 1.000 2.000
#> 565   7 19   0  0  0   1  0  0 0.246 0.328 0.474 0.802
#> 566  14 42   1  1  0   3  4  2 0.228 0.312 0.488 0.799
#> 567  10 15   0  1  1   4  7  0 0.321 0.366 0.433 0.798
#> 568   8 33   0  3  3   2  2  1 0.225 0.273 0.325 0.598
#> 569  18 39   2  1  0   3  3  1 0.338 0.421 0.547 0.968
#> 570  14 44   1  0  0   5  1  1 0.271 0.352 0.688 1.039
#> 571   3  6   1  0  0   2  0  0 0.278 0.350 0.417 0.767
#> 572   2 31   1  0  0   1 11  3 0.292 0.308 0.351 0.659
#> 573   0  1   0  2  0   0  0  0 0.000 0.000 0.000 0.000
#> 574   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 575   1 14   0  0  0   0  0  0 0.174 0.208 0.217 0.426
#> 576   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 577   2  6   1  1  0   1  1  0 0.185 0.267 0.370 0.637
#> 578   0  3   0  0  0   0  0  0 0.100 0.100 0.100 0.200
#> 579   7 13   0  0  0   0  0  0 0.244 0.354 0.293 0.647
#> 580   6  9   0  0  0   0  4  1 0.305 0.369 0.339 0.708
#> 581   5 11   2  0  1   4  0  0 0.229 0.287 0.305 0.592
#> 582   6 22   0  1  0   6  1  1 0.274 0.317 0.368 0.685
#> 583   7 22   1  0  1   2  0  0 0.171 0.250 0.280 0.530
#> 584   0  8   0  2  0   0  0  0 0.308 0.308 0.538 0.846
#> 585   0  7   0  0  0   0  0  0 0.200 0.200 0.250 0.450
#> 586   1  6   1  0  0   3  0  0 0.313 0.353 0.313 0.665
#> 587   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 588   4 12   0  0  0   0  1  1 0.156 0.224 0.222 0.447
#> 589   6 34   0  0  1   1  4  1 0.210 0.266 0.320 0.586
#> 590   0  1   0  2  0   0  0  0 0.083 0.083 0.083 0.167
#> 591  18 22   3  0  1   0  1  0 0.277 0.387 0.538 0.925
#> 592   9 17   1  0  1   1  1  0 0.286 0.353 0.407 0.760
#> 593   0  1   0  0  0   1  0  0 0.250 0.250 1.000 1.250
#> 594   3  0   0  0  0   1  0  0 0.353 0.450 0.471 0.921
#> 595   1  0   0  0  0   0  0  0    NA 1.000    NA    NA
#> 596   3  4   0  0  0   0  2  0 0.308 0.379 0.462 0.841
#> 597   4 32   1  0  0   5  2  1 0.331 0.356 0.480 0.836
#> 598   1  7   0  0  0   1  0  0 0.200 0.231 0.320 0.551
#> 599   0  0   0  2  0   0  0  0    NA    NA    NA    NA
#> 600  11 35   1  0  1   2  1  3 0.297 0.368 0.566 0.934
#> 601   7 35   0  0  0   2  0  0 0.209 0.250 0.380 0.630
#> 602   8 27   1  0  0   2  0  0 0.269 0.327 0.481 0.808
#> 603   7 31   0  0  1   5  0  0 0.300 0.343 0.454 0.797
#> 604   9 13   3  0  0   3  1  1 0.333 0.402 0.524 0.926
#> 605   5  7   0  0  0   0  0  0 0.250 0.400 0.350 0.750
#> 606   3  6   0  0  0   0  3  0 0.182 0.280 0.409 0.689
#> 607  20 44   1  0  1   1  7  0 0.259 0.362 0.381 0.743
#> 608   0  6   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 609   8 18   1  0  0   2  1  0 0.300 0.370 0.411 0.781
#> 610   3  5   0  0  0   4  0  0 0.242 0.306 0.333 0.639
#> 611  13 16   1  0  3   2  2  0 0.238 0.314 0.336 0.650
#> 612  13 35   1  0  1   5  0  0 0.164 0.248 0.426 0.674
#> 613   3 12   0  1  1   0  1  1 0.310 0.339 0.586 0.925
#> 614   3 13   0  0  1   1  0  1 0.245 0.283 0.429 0.712
#> 615   0 18   0  0  0   3  0  0 0.191 0.191 0.319 0.511
#> 616  11 31   0  0  0   3  6  0 0.283 0.341 0.394 0.734
#> 617   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 618   3  2   0  0  0   0  1  0 0.450 0.522 0.650 1.172
#> 619   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 620   5 21   0  1  1   1  1  2 0.306 0.353 0.468 0.821
#> 621   0  4   0  2  0   0  0  0 0.133 0.133 0.200 0.333
#> 622  20 34   0  0  6   1  0  0 0.271 0.374 0.492 0.866
#> 623   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 624  21 29   1  0  0   1  1  0 0.271 0.382 0.511 0.893
#> 625   0  3   0  2  0   0  0  0 0.100 0.100 0.100 0.200
#> 626   1  4   0  0  0   0  0  0 0.000 0.125 0.000 0.125
#> 627   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 628   6 31   1  0  0   3  0  0 0.221 0.269 0.336 0.605
#> 629   0  2   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 630   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 631   0  9   0  0  0   0  0  0 0.167 0.167 0.167 0.333
#> 632   0  1   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 633   0  3   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 634   3 10   1  0  0   1  0  0 0.111 0.200 0.111 0.311
#> 635   3  4   1  0  1   0  0  0 0.250 0.360 0.400 0.760
#> 636   2  9   0  0  2   1  0  0 0.314 0.333 0.657 0.990
#> 637   1  3   0  0  0   0  0  0 0.182 0.250 0.273 0.523
#> 638   1  3   0  0  0   0  0  0 0.286 0.318 0.571 0.890
#> 639   1 11   0  0  0   1  0  0 0.120 0.154 0.120 0.274
#> 640   0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 641   0  6   0  0  0   0  0  0 0.438 0.438 0.500 0.938
#> 642   0  0   0  1  0   0  0  0    NA    NA    NA    NA
#> 643   0  0   0  0  0   0  0  0 0.000 0.000 0.000 0.000
#> 644  11 26   3  0  1   2  3  3 0.245 0.309 0.415 0.724
#> 645   0  9   0  2  0   1  0  0 0.235 0.235 0.235 0.471
#> 646   0  2   0  0  0   0  0  0 0.200 0.200 0.200 0.400
#> 647   0  1   0  0  0   0  0  0 0.333 0.333 0.333 0.667
#> 648   0  1   0  1  0   0  0  0 0.500 0.500 0.500 1.000
#> 649  13 35   1  0  0   3  4  1 0.259 0.327 0.388 0.715
#> 650   2 12   0  0  0   1  0  0 0.277 0.306 0.319 0.625
#> 651   0  0   0  0  0   0  0  0 0.667 0.667 0.667 1.333
#> 652   1 16   0  0  0   1  0  0 0.176 0.188 0.294 0.483
#> 653   3 21   0  0  1   5  0  0 0.240 0.260 0.333 0.593
#> 654   0  3   0  1  0   0  0  0 0.200 0.200 0.200 0.400
#> 655   7 18   1  0  3   8  1  0 0.189 0.248 0.300 0.548
#> 656   0  2   0  2  0   0  0  0 0.214 0.214 0.286 0.500
#> 657  12 10   0  0  0   2  0  0 0.244 0.340 0.463 0.804
#> 658   9 43   0  1  0   1  0  1 0.157 0.225 0.304 0.529
# }
```
