# **Calculate FIP and related metrics for any set of data**

This function allows you to calculate FIP and related metrics for any
given set of data, provided the right variables are in the data set. The
function currently returns both FIP per inning pitched, wOBA against
(based on batters faced), and wOBA against per instance of fair contact.

## Usage

``` r
fip_plus(df)
```

## Arguments

- df:

  A data frame of statistics that includes, at a minimum, the following
  columns: IP (innings pitched), BF (batters faced), uBB (unintentional
  walks), HBP (Hit By Pitch), x1B (singles), x2B (doubles), x3B
  (triples), HR (home runs), AB (at-bats), SH (sacrifice hits), SO
  (strike outs), and season.

## Value

Returns a tibble with the following columns:

|                  |           |
|------------------|-----------|
| col_name         | types     |
| bbref_id         | character |
| season           | integer   |
| Name             | character |
| Age              | numeric   |
| Level            | character |
| Team             | character |
| G                | numeric   |
| GS               | numeric   |
| W                | numeric   |
| L                | numeric   |
| SV               | numeric   |
| IP               | numeric   |
| H                | numeric   |
| R                | numeric   |
| ER               | numeric   |
| uBB              | numeric   |
| BB               | numeric   |
| SO               | numeric   |
| HR               | numeric   |
| HBP              | numeric   |
| ERA              | numeric   |
| AB               | numeric   |
| X1B              | numeric   |
| X2B              | numeric   |
| X3B              | numeric   |
| IBB              | numeric   |
| GDP              | numeric   |
| SF               | numeric   |
| SB               | numeric   |
| CS               | numeric   |
| PO               | numeric   |
| BF               | numeric   |
| Pit              | numeric   |
| Str              | numeric   |
| StL              | numeric   |
| StS              | numeric   |
| GB.FB            | numeric   |
| LD               | numeric   |
| PU               | numeric   |
| WHIP             | numeric   |
| BAbip            | numeric   |
| SO9              | numeric   |
| SO.W             | numeric   |
| SO_perc          | numeric   |
| uBB_perc         | numeric   |
| SO_uBB           | numeric   |
| FIP              | numeric   |
| wOBA_against     | numeric   |
| wOBA_CON_against | numeric   |

## Examples

``` r
# \donttest{
  try({
    df <- bref_daily_pitcher("2015-04-05", "2015-04-30")
    fip_plus(df)
  })
#> ✖ 2026-06-08 01:56:35.382181: Invalid arguments or no daily pitcher data available!
#>     season                  Name Age         Level                 Team
#> 1     2015         Heath Hembree  26        Maj-AL               Boston
#> 2     2015         Derek Holland  28        Maj-AL                Texas
#> 3     2015           Jon Edwards  27        Maj-AL                Texas
#> 4     2015          Cory Mazzoni  25        Maj-NL            San Diego
#> 5     2015     Severino González  22        Maj-NL         Philadelphia
#> 6     2015          John Cornely  26        Maj-NL              Atlanta
#> 7     2015         Chris Rearick  27        Maj-NL            San Diego
#> 8     2015           Donnie Veal  30        Maj-NL              Atlanta
#> 9     2015          Jorge Rondon  27        Maj-NL             Colorado
#> 10    2015       Anthony Ranaudo  25        Maj-AL                Texas
#> 11    2015           Scott Oberg  25        Maj-NL             Colorado
#> 12    2015            Tim Cooney  24        Maj-NL            St. Louis
#> 13    2015      Jack Leathersich  24        Maj-NL             New York
#> 14    2015     C.J. Riefenhauser  25        Maj-AL            Tampa Bay
#> 15    2015         Jeff Beliveau  28        Maj-AL            Tampa Bay
#> 16    2015             A.J. Cole  23        Maj-NL           Washington
#> 17    2015           Ian Kennedy  30        Maj-NL            San Diego
#> 18    2015      Michael Lorenzen  23        Maj-NL           Cincinnati
#> 19    2015         Drew Rucinski  26        Maj-AL          Los Angeles
#> 20    2015         Sergio Santos  31        Maj-NL          Los Angeles
#> 21    2015            David Huff  30        Maj-NL          Los Angeles
#> 22    2015         Ross Detwiler  29        Maj-AL                Texas
#> 23    2015           Arnold Leon  26        Maj-AL              Oakland
#> 24    2015         Rafael Martín  31        Maj-NL           Washington
#> 25    2015       Brian Schlitter  29        Maj-NL              Chicago
#> 26    2015          Shawn Kelley  31        Maj-NL            San Diego
#> 27    2015        Burke Badenhop  32        Maj-NL           Cincinnati
#> 28    2015          Tim Stauffer  33        Maj-AL            Minnesota
#> 29    2015         Ricky Nolasco  32        Maj-AL            Minnesota
#> 30    2015           Josh Fields  29        Maj-AL              Houston
#> 31    2015        Ryan Vogelsong  37        Maj-NL        San Francisco
#> 32    2015      Jorge De La Rosa  34        Maj-NL             Colorado
#> 33    2015            Colt Hynes  30        Maj-AL              Toronto
#> 34    2015       Al Alburquerque  29        Maj-AL              Detroit
#> 35    2015         Kyle Kendrick  30        Maj-NL             Colorado
#> 36    2015        Felipe Vazquez  23        Maj-NL           Washington
#> 37    2015       Everett Teaford  31        Maj-AL            Tampa Bay
#> 38    2015      Brandon Finnegan  22        Maj-AL          Kansas City
#> 39    2015             Adam Wilk  27        Maj-AL          Los Angeles
#> 40    2015        LaTroy Hawkins  42        Maj-NL             Colorado
#> 41    2015        Erasmo Ramírez  25        Maj-AL            Tampa Bay
#> 42    2015        Branden Pinder  26        Maj-AL             New York
#> 43    2015            Cody Allen  26        Maj-AL            Cleveland
#> 44    2015          Óliver Pérez  33        Maj-NL              Arizona
#> 45    2015       Tyler Thornburg  26        Maj-NL            Milwaukee
#> 46    2015          Todd Redmond  30        Maj-AL              Toronto
#> 47    2015         Steven Wright  30        Maj-AL               Boston
#> 48    2015           Kyle Drabek  27        Maj-AL              Chicago
#> 49    2015            Mike Fiers  30        Maj-NL            Milwaukee
#> 50    2015            Bud Norris  30        Maj-AL            Baltimore
#> 51    2015          Jake Diekman  28        Maj-NL         Philadelphia
#> 52    2015      Kendall Graveman  24        Maj-AL              Oakland
#> 53    2015   Asher Wojciechowski  26        Maj-AL              Houston
#> 54    2015            T.J. House  25        Maj-AL            Cleveland
#> 55    2015         Edward Mujica  31        Maj-AL               Boston
#> 56    2015           Kirby Yates  28        Maj-AL            Tampa Bay
#> 57    2015          R.J. Alvarez  24        Maj-AL              Oakland
#> 58    2015         Evan Marshall  25        Maj-NL              Arizona
#> 59    2015          Homer Bailey  29        Maj-NL           Cincinnati
#> 60    2015         Chris Tillman  27        Maj-AL            Baltimore
#> 61    2015            Jake Peavy  34        Maj-NL        San Francisco
#> 62    2015             Phil Coke  32        Maj-NL              Chicago
#> 63    2015       Eric O'Flaherty  30        Maj-AL              Oakland
#> 64    2015          Mark Buehrle  36        Maj-AL              Toronto
#> 65    2015           Manny Parra  32        Maj-NL           Cincinnati
#> 66    2015      Tanner Scheppers  28        Maj-AL                Texas
#> 67    2015       Hisashi Iwakuma  34        Maj-AL              Seattle
#> 68    2015        David Buchanan  26        Maj-NL         Philadelphia
#> 69    2015            José Ureña  23        Maj-NL                Miami
#> 70    2015          Randy Choate  39        Maj-NL            St. Louis
#> 71    2015           J.R. Graham  25        Maj-AL            Minnesota
#> 72    2015         Jake Petricka  27        Maj-AL              Chicago
#> 73    2015         Jose Quintana  26        Maj-AL              Chicago
#> 74    2015         Julio Teheran  24        Maj-NL              Atlanta
#> 75    2015             Mat Latos  27        Maj-NL                Miami
#> 76    2015      Brandon McCarthy  31        Maj-NL          Los Angeles
#> 77    2015            Bryan Shaw  27        Maj-AL            Cleveland
#> 78    2015        Brian Duensing  32        Maj-AL            Minnesota
#> 79    2015        Ernesto Frieri  29        Maj-AL            Tampa Bay
#> 80    2015      Antonio Bastardo  29        Maj-NL           Pittsburgh
#> 81    2015          Nick Vincent  28        Maj-NL            San Diego
#> 82    2015            Brad Brach  29        Maj-AL            Baltimore
#> 83    2015          Jason García  22        Maj-AL            Baltimore
#> 84    2015          A.J. Schugel  26        Maj-NL              Arizona
#> 85    2015        Matt Shoemaker  28        Maj-AL          Los Angeles
#> 86    2015        Nathan Eovaldi  25        Maj-AL             New York
#> 87    2015          Jason Vargas  32        Maj-AL          Kansas City
#> 88    2015         Aaron Sanchez  22        Maj-AL              Toronto
#> 89    2015        Jeremy Guthrie  36        Maj-AL          Kansas City
#> 90    2015        Yusmeiro Petit  30        Maj-NL        San Francisco
#> 91    2015       Gonzalez Germen  27        Maj-NL              Chicago
#> 92    2015          Gio Gonzalez  29        Maj-NL           Washington
#> 93    2015        Tom Wilhelmsen  31        Maj-AL              Seattle
#> 94    2015            Jumbo Díaz  31        Maj-NL           Cincinnati
#> 95    2015            Kyle Lohse  36        Maj-NL            Milwaukee
#> 96    2015         Miguel Castro  20        Maj-AL              Toronto
#> 97    2015         Grant Balfour  37        Maj-AL            Tampa Bay
#> 98    2015         Trevor Cahill  27        Maj-NL              Atlanta
#> 99    2015          Addison Reed  26        Maj-NL              Arizona
#> 100   2015           Robbie Ross  26        Maj-AL               Boston
#> 101   2015     Jeremy Hellickson  28        Maj-NL              Arizona
#> 102   2015             Jon Niese  28        Maj-NL             New York
#> 103   2015          Wily Peralta  26        Maj-NL            Milwaukee
#> 104   2015          Eddie Butler  24        Maj-NL             Colorado
#> 105   2015              Ian Krol  24        Maj-AL              Detroit
#> 106   2015           Brett Cecil  28        Maj-AL              Toronto
#> 107   2015           Kyle Gibson  27        Maj-AL            Minnesota
#> 108   2015         Matt Andriese  25        Maj-AL            Tampa Bay
#> 109   2015            John Danks  30        Maj-AL              Chicago
#> 110   2015           Kevin Gregg  37        Maj-NL           Cincinnati
#> 111   2015         Jason Marquis  36        Maj-NL           Cincinnati
#> 112   2015       Anthony Swarzak  29        Maj-AL            Cleveland
#> 113   2015            Matt Garza  31        Maj-NL            Milwaukee
#> 114   2015        Brett Anderson  27        Maj-NL          Los Angeles
#> 115   2015          Tanner Roark  28        Maj-NL           Washington
#> 116   2015         Logan Verrett  25        Maj-AL                Texas
#> 117   2015         Daniel Norris  22        Maj-AL              Toronto
#> 118   2015        Taijuan Walker  22        Maj-AL              Seattle
#> 119   2015           Javy Guerra  29        Maj-AL              Chicago
#> 120   2015            Wade Miley  28        Maj-AL               Boston
#> 121   2015            Jon Lester  31        Maj-NL              Chicago
#> 122   2015          Jered Weaver  32        Maj-AL          Los Angeles
#> 123   2015         Daniel Hudson  28        Maj-NL              Arizona
#> 124   2015            Pedro Báez  27        Maj-NL          Los Angeles
#> 125   2015       Zach McAllister  27        Maj-AL            Cleveland
#> 126   2015      Santiago Casilla  34        Maj-NL        San Francisco
#> 127   2015          Brad Peacock  27        Maj-AL              Houston
#> 128   2015       Wandy Rodríguez  36        Maj-AL                Texas
#> 129   2015       Jerome Williams  33        Maj-NL         Philadelphia
#> 130   2015           J.P. Howell  32        Maj-NL          Los Angeles
#> 131   2015           Phil Hughes  29        Maj-AL            Minnesota
#> 132   2015     Sugar Ray Marimón  26        Maj-NL              Atlanta
#> 133   2015       Anthony Varvaro  30        Maj-AL               Boston
#> 134   2015          Blaine Boyer  33        Maj-AL            Minnesota
#> 135   2015            Keone Kela  22        Maj-AL                Texas
#> 136   2015             Ryan Cook  28        Maj-AL              Oakland
#> 137   2015        Drew Hutchison  24        Maj-AL              Toronto
#> 138   2015           Jim Johnson  32        Maj-NL              Atlanta
#> 139   2015       Jeff Samardzija  30        Maj-AL              Chicago
#> 140   2015      Jonathan Broxton  31        Maj-NL            Milwaukee
#> 141   2015          Steve Cishek  29        Maj-NL                Miami
#> 142   2015         Craig Kimbrel  27        Maj-NL            San Diego
#> 143   2015         Kevin Gausman  24        Maj-AL            Baltimore
#> 144   2015          Tommy Milone  28        Maj-AL            Minnesota
#> 145   2015           CC Sabathia  34        Maj-AL             New York
#> 146   2015            Chris Sale  26        Maj-AL              Chicago
#> 147   2015           Boone Logan  30        Maj-NL             Colorado
#> 148   2015          Hector Neris  26        Maj-NL         Philadelphia
#> 149   2015             Mike Dunn  30        Maj-NL                Miami
#> 150   2015          Tyler Matzek  24        Maj-NL             Colorado
#> 151   2015          Radhames Liz  31        Maj-NL           Pittsburgh
#> 152   2015           Nick Masset  33        Maj-NL                Miami
#> 153   2015         Mark Melancon  30        Maj-NL           Pittsburgh
#> 154   2015         Blake Treinen  27        Maj-NL           Washington
#> 155   2015           R.A. Dickey  40        Maj-AL              Toronto
#> 156   2015           Colby Lewis  35        Maj-AL                Texas
#> 157   2015            Tyson Ross  28        Maj-NL            San Diego
#> 158   2015            Ian Thomas  28        Maj-NL              Atlanta
#> 159   2015          Blaine Hardy  28        Maj-AL              Detroit
#> 160   2015            Phil Klein  26        Maj-AL                Texas
#> 161   2015     Stephen Strasburg  26        Maj-NL           Washington
#> 162   2015        Yoervis Medina  26        Maj-AL              Seattle
#> 163   2015         Xavier Cedeño  28 Maj-AL,Maj-NL Tampa Bay,Washington
#> 164   2015       Raisel Iglesias  25        Maj-NL           Cincinnati
#> 165   2015          Brian Matusz  28        Maj-AL            Baltimore
#> 166   2015         Fernando Abad  29        Maj-AL              Oakland
#> 167   2015          Vance Worley  27        Maj-NL           Pittsburgh
#> 168   2015        Fernando Salas  30        Maj-AL          Los Angeles
#> 169   2015        Vinnie Pestano  30        Maj-AL          Los Angeles
#> 170   2015          Roenis Elías  26        Maj-AL              Seattle
#> 171   2015       Yordano Ventura  24        Maj-AL          Kansas City
#> 172   2015             Alex Wood  24        Maj-NL              Atlanta
#> 173   2015        Andrew Cashner  28        Maj-NL            San Diego
#> 174   2015      Rubby De La Rosa  26        Maj-NL              Arizona
#> 175   2015          Jordan Lyles  24        Maj-NL             Colorado
#> 176   2015            Pat Neshek  34        Maj-AL              Houston
#> 177   2015          Casey Sadler  24        Maj-NL           Pittsburgh
#> 178   2015         Clay Buchholz  30        Maj-AL               Boston
#> 179   2015         Rick Porcello  26        Maj-AL               Boston
#> 180   2015         Scott Feldman  32        Maj-AL              Houston
#> 181   2015        Aníbal Sánchez  31        Maj-AL              Detroit
#> 182   2015            Matt Tracy  26        Maj-AL             New York
#> 183   2015           Danny Duffy  26        Maj-AL          Kansas City
#> 184   2015         Neftalí Feliz  27        Maj-AL                Texas
#> 185   2015           Doug Fister  31        Maj-NL           Washington
#> 186   2015            Tim Hudson  39        Maj-NL        San Francisco
#> 187   2015          James Paxton  26        Maj-AL              Seattle
#> 188   2015         Danny Salazar  25        Maj-AL            Cleveland
#> 189   2015       Yovani Gallardo  29        Maj-AL                Texas
#> 190   2015           Tom Koehler  29        Maj-NL                Miami
#> 191   2015            Jeff Locke  27        Maj-NL           Pittsburgh
#> 192   2015          Carlos Rodón  22        Maj-AL              Chicago
#> 193   2015          Chris Heston  27        Maj-NL        San Francisco
#> 194   2015          Tommy Hunter  28        Maj-AL            Baltimore
#> 195   2015       Clayton Kershaw  27        Maj-NL          Los Angeles
#> 196   2015            Lance Lynn  28        Maj-NL            St. Louis
#> 197   2015          Bryan Morris  28        Maj-NL                Miami
#> 198   2015         Drew Pomeranz  26        Maj-AL              Oakland
#> 199   2015   Christian Friedrich  27        Maj-NL             Colorado
#> 200   2015          Tim Lincecum  31        Maj-NL        San Francisco
#> 201   2015        Chase Anderson  27        Maj-NL              Arizona
#> 202   2015          Héctor Noesí  28        Maj-AL              Chicago
#> 203   2015           Tyler Olson  25        Maj-AL              Seattle
#> 204   2015            Dillon Gee  29        Maj-NL             New York
#> 205   2015      Justin Masterson  30        Maj-AL               Boston
#> 206   2015       Sean O'Sullivan  27        Maj-NL         Philadelphia
#> 207   2015           Eric Stults  35        Maj-NL              Atlanta
#> 208   2015         Cam Bedrosian  23        Maj-AL          Los Angeles
#> 209   2015           Scott Baker  33        Maj-NL          Los Angeles
#> 210   2015             Brad Hand  25        Maj-NL                Miami
#> 211   2015            Nate Karns  27        Maj-AL            Tampa Bay
#> 212   2015            Aaron Loup  27        Maj-AL              Toronto
#> 213   2015        Dustin McGowan  33        Maj-NL         Philadelphia
#> 214   2015           Adam Warren  27        Maj-AL             New York
#> 215   2015     Jordan Zimmermann  29        Maj-NL           Washington
#> 216   2015       Miguel González  31        Maj-AL            Baltimore
#> 217   2015         Hansel Robles  24        Maj-NL             New York
#> 218   2015         Chase Whitley  26        Maj-AL             New York
#> 219   2015          José Álvarez  26        Maj-AL          Los Angeles
#> 220   2015        Kyle Hendricks  25        Maj-NL              Chicago
#> 221   2015         Wesley Wright  30        Maj-AL            Baltimore
#> 222   2015       Jeremy Jeffress  27        Maj-NL            Milwaukee
#> 223   2015     Madison Bumgarner  25        Maj-NL        San Francisco
#> 224   2015       Carlos Carrasco  28        Maj-AL            Cleveland
#> 225   2015          Jacob deGrom  27        Maj-NL             New York
#> 226   2015         James Shields  33        Maj-NL            San Diego
#> 227   2015      Joba Chamberlain  29        Maj-AL              Detroit
#> 228   2015       Fernando Rodney  38        Maj-AL              Seattle
#> 229   2015        Joaquín Benoit  37        Maj-NL            San Diego
#> 230   2015         Andrew Chafin  25        Maj-NL              Arizona
#> 231   2015      Justin De Fratus  27        Maj-NL         Philadelphia
#> 232   2015           Cody Martin  25        Maj-NL              Atlanta
#> 233   2015        Danny Farquhar  28        Maj-AL              Seattle
#> 234   2015         Nick Hagadone  29        Maj-AL            Cleveland
#> 235   2015     Roberto Hernández  34        Maj-AL              Houston
#> 236   2015          Shane Greene  26        Maj-AL              Detroit
#> 237   2015     Christian Bergman  27        Maj-NL             Colorado
#> 238   2015            Trevor May  25        Maj-AL            Minnesota
#> 239   2015        Michael Pineda  26        Maj-AL             New York
#> 240   2015         Jeanmar Gómez  27        Maj-NL         Philadelphia
#> 241   2015          Corey Kluber  29        Maj-AL            Cleveland
#> 242   2015           Cole Hamels  31        Maj-NL         Philadelphia
#> 243   2015            Rob Wooten  29        Maj-NL            Milwaukee
#> 244   2015       Josh Collmenter  29        Maj-NL              Arizona
#> 245   2015         Alfredo Simón  34        Maj-AL              Detroit
#> 246   2015 Henderson Alvarez III  25        Maj-NL                Miami
#> 247   2015           Tommy Layne  30        Maj-AL               Boston
#> 248   2015             Dan Otero  30        Maj-AL              Oakland
#> 249   2015        Brandon Maurer  24        Maj-NL            San Diego
#> 250   2015     Kevin Quackenbush  26        Maj-NL            San Diego
#> 251   2015          Wei-Yin Chen  29        Maj-AL            Baltimore
#> 252   2015            Neal Cotts  35        Maj-NL            Milwaukee
#> 253   2015        Sean Gilmartin  25        Maj-NL             New York
#> 254   2015          Jeff Francis  34        Maj-AL              Toronto
#> 255   2015             Ken Giles  24        Maj-NL         Philadelphia
#> 256   2015         Chris Hatcher  30        Maj-NL          Los Angeles
#> 257   2015         Kyle Lobstein  25        Maj-AL              Detroit
#> 258   2015         Carlos Torres  32        Maj-NL             New York
#> 259   2015          Jared Hughes  29        Maj-NL           Pittsburgh
#> 260   2015          Mike Pelfrey  31        Maj-AL            Minnesota
#> 261   2015         Buddy Carlyle  37        Maj-NL             New York
#> 262   2015           John Lackey  36        Maj-NL            St. Louis
#> 263   2015         Dominic Leone  23        Maj-AL              Seattle
#> 264   2015           Cesár Ramos  31        Maj-AL          Los Angeles
#> 265   2015            Drew Smyly  26        Maj-AL            Tampa Bay
#> 266   2015       Héctor Santiago  27        Maj-AL          Los Angeles
#> 267   2015         Tony Cingrani  25        Maj-NL           Cincinnati
#> 268   2015         Bartolo Colón  42        Maj-NL             New York
#> 269   2015           David Price  29        Maj-AL              Detroit
#> 270   2015           Alex Wilson  28        Maj-AL              Detroit
#> 271   2015        Brandon Morrow  30        Maj-NL            San Diego
#> 272   2015           Luis Garcia  28        Maj-NL         Philadelphia
#> 273   2015             Joe Kelly  27        Maj-AL               Boston
#> 274   2015        Tyler Clippard  30        Maj-AL              Oakland
#> 275   2015            Sam Deduno  31        Maj-AL              Houston
#> 276   2015             J.A. Happ  32        Maj-AL              Seattle
#> 277   2015         Collin McHugh  28        Maj-AL              Houston
#> 278   2015           Koji Uehara  40        Maj-AL               Boston
#> 279   2015          A.J. Burnett  38        Maj-NL           Pittsburgh
#> 280   2015          Jason Hammel  32        Maj-NL              Chicago
#> 281   2015           Rob Scahill  28        Maj-NL           Pittsburgh
#> 282   2015             Dan Haren  34        Maj-NL                Miami
#> 283   2015          Román Méndez  24        Maj-AL                Texas
#> 284   2015          Brooks Brown  30        Maj-NL             Colorado
#> 285   2015            Mike Morin  24        Maj-AL          Los Angeles
#> 286   2015          Mitch Harris  29        Maj-NL            St. Louis
#> 287   2015         Shelby Miller  24        Maj-NL              Atlanta
#> 288   2015          Joe Thatcher  33        Maj-AL              Houston
#> 289   2015           Matt Albers  32        Maj-AL              Chicago
#> 290   2015         Craig Breslow  34        Maj-AL               Boston
#> 291   2015          Jimmy Nelson  26        Maj-NL            Milwaukee
#> 292   2015        Tom Gorzelanny  32        Maj-AL              Detroit
#> 293   2015           Kyle Davies  31        Maj-AL             New York
#> 294   2015           Travis Wood  28        Maj-NL              Chicago
#> 295   2015           Seth Maness  26        Maj-NL            St. Louis
#> 296   2015          Jason Grilli  38        Maj-NL              Atlanta
#> 297   2015         Héctor Rondón  27        Maj-NL              Chicago
#> 298   2015          Austin Adams  28        Maj-AL            Cleveland
#> 299   2015            Casey Fien  31        Maj-AL            Minnesota
#> 300   2015          Frank Garcés  25        Maj-NL            San Diego
#> 301   2015       Carlos Martínez  23        Maj-NL            St. Louis
#> 302   2015      Garrett Richards  27        Maj-AL          Los Angeles
#> 303   2015         Chasen Shreve  24        Maj-AL             New York
#> 304   2015             Zach Duke  32        Maj-AL              Chicago
#> 305   2015           Matt Harvey  26        Maj-NL             New York
#> 306   2015            Juan Jaime  27        Maj-NL              Atlanta
#> 307   2015       Adam Wainwright  33        Maj-NL            St. Louis
#> 308   2015          Shaun Marcum  33        Maj-AL            Cleveland
#> 309   2015           Dale Thayer  34        Maj-NL            San Diego
#> 310   2015        Scott Atchison  39        Maj-AL            Cleveland
#> 311   2015         Jarred Cosart  25        Maj-NL                Miami
#> 312   2015            Jesse Hahn  25        Maj-AL              Oakland
#> 313   2015           J.J. Hoover  27        Maj-NL           Cincinnati
#> 314   2015          Anthony Bass  27        Maj-AL                Texas
#> 315   2015          Juan Nicasio  28        Maj-NL          Los Angeles
#> 316   2015         Michael Wacha  23        Maj-NL            St. Louis
#> 317   2015           C.J. Wilson  34        Maj-AL          Los Angeles
#> 318   2015         Nick Martinez  24        Maj-AL                Texas
#> 319   2015           Drew Storen  27        Maj-NL           Washington
#> 320   2015        Junichi Tazawa  29        Maj-AL               Boston
#> 321   2015        Mike Bolsinger  27        Maj-NL          Los Angeles
#> 322   2015       Randall Delgado  25        Maj-NL              Arizona
#> 323   2015         Liam Hendriks  26        Maj-AL              Toronto
#> 324   2015        Kevin Siegrist  25        Maj-NL            St. Louis
#> 325   2015   Odrisamer Despaigne  28        Maj-NL            San Diego
#> 326   2015          Michael Kohn  29        Maj-NL              Atlanta
#> 327   2015           Zach Putnam  27        Maj-AL              Chicago
#> 328   2015   Francisco Rodríguez  33        Maj-NL            Milwaukee
#> 329   2015          Trevor Bauer  24        Maj-AL            Cleveland
#> 330   2015          Jason Frasor  37        Maj-AL          Kansas City
#> 331   2015           Jason Motte  33        Maj-NL              Chicago
#> 332   2015           Chad Qualls  36        Maj-AL              Houston
#> 333   2015          Esmil Rogers  29        Maj-AL             New York
#> 334   2015          Jesse Chavez  31        Maj-AL              Oakland
#> 335   2015          Erik Goeddel  26        Maj-NL             New York
#> 336   2015          Carlos Frías  25        Maj-NL          Los Angeles
#> 337   2015          Alexi Ogando  31        Maj-AL               Boston
#> 338   2015        Rafael Montero  24        Maj-NL             New York
#> 339   2015          Aaron Harang  37        Maj-NL         Philadelphia
#> 340   2015          David Phelps  28        Maj-NL                Miami
#> 341   2015          Zack Greinke  31        Maj-NL          Los Angeles
#> 342   2015            Mike Leake  27        Maj-NL           Cincinnati
#> 343   2015         Chris Bassitt  26        Maj-AL              Oakland
#> 344   2015           Steve Geltz  27        Maj-AL            Tampa Bay
#> 345   2015        Caleb Thielbar  28        Maj-AL            Minnesota
#> 346   2015       Masahiro Tanaka  26        Maj-AL             New York
#> 347   2015         Brandon Gomes  30        Maj-AL            Tampa Bay
#> 348   2015       Stolmy Pimentel  25        Maj-AL                Texas
#> 349   2015            Jean Machi  33        Maj-NL        San Francisco
#> 350   2015          Zack Britton  27        Maj-AL            Baltimore
#> 351   2015   Arquimedes Caminero  28        Maj-NL           Pittsburgh
#> 352   2015           Luis Avilán  25        Maj-NL              Atlanta
#> 353   2015        Brad Boxberger  27        Maj-AL            Tampa Bay
#> 354   2015      Andrew McKirahan  25        Maj-NL              Atlanta
#> 355   2015         Ángel Nesbitt  24        Maj-AL              Detroit
#> 356   2015             Sam Dyson  27        Maj-NL                Miami
#> 357   2015         Kyle Crockett  23        Maj-AL            Cleveland
#> 358   2015         Roberto Osuna  20        Maj-AL              Toronto
#> 359   2015           Gerrit Cole  24        Maj-NL           Pittsburgh
#> 360   2015         George Kontos  30        Maj-NL        San Francisco
#> 361   2015         Evan Scribner  29        Maj-AL              Oakland
#> 362   2015         Nick Tropeano  24        Maj-AL          Los Angeles
#> 363   2015           Tony Watson  30        Maj-NL           Pittsburgh
#> 364   2015           Alex Torres  27        Maj-NL             New York
#> 365   2015        Jeremy Affeldt  36        Maj-NL        San Francisco
#> 366   2015          Jake Arrieta  29        Maj-NL              Chicago
#> 367   2015         Marco Estrada  31        Maj-AL              Toronto
#> 368   2015       Félix Hernández  29        Maj-AL              Seattle
#> 369   2015             Joe Smith  31        Maj-AL          Los Angeles
#> 370   2015        Paco Rodríguez  24        Maj-NL          Los Angeles
#> 371   2015      Pedro Villarreal  27        Maj-NL           Cincinnati
#> 372   2015     Francisco Liriano  31        Maj-NL           Pittsburgh
#> 373   2015         Aaron Barrett  27        Maj-NL           Washington
#> 374   2015           Matt Barnes  25        Maj-AL               Boston
#> 375   2015        Archie Bradley  22        Maj-NL              Arizona
#> 376   2015              AJ Ramos  28        Maj-NL                Miami
#> 377   2015        Aaron Thompson  28        Maj-AL            Minnesota
#> 378   2015            Sonny Gray  25        Maj-AL              Oakland
#> 379   2015            Will Smith  25        Maj-NL            Milwaukee
#> 380   2015           Ryan Madson  34        Maj-AL          Kansas City
#> 381   2015         Jordan Walden  27        Maj-NL            St. Louis
#> 382   2015             Ryan Webb  29        Maj-AL            Cleveland
#> 383   2015         Jake Odorizzi  25        Maj-AL            Tampa Bay
#> 384   2015       Charlie Furbush  29        Maj-AL              Seattle
#> 385   2015          Darren O'Day  32        Maj-AL            Baltimore
#> 386   2015      Marc Rzepczynski  29        Maj-AL            Cleveland
#> 387   2015          Johnny Cueto  29        Maj-NL           Cincinnati
#> 388   2015        Kelvin Herrera  25        Maj-AL          Kansas City
#> 389   2015       Edinson Vólquez  31        Maj-AL          Kansas City
#> 390   2015          Scott Kazmir  31        Maj-AL              Oakland
#> 391   2015        Ubaldo Jiménez  31        Maj-AL            Baltimore
#> 392   2015         Craig Stammen  31        Maj-NL           Washington
#> 393   2015        Shawn Tolleson  27        Maj-AL                Texas
#> 394   2015          Chris Archer  26        Maj-AL            Tampa Bay
#> 395   2015         Edwin Jackson  31        Maj-NL              Chicago
#> 396   2015       David Carpenter  29        Maj-AL             New York
#> 397   2015     Jonathan Papelbon  34        Maj-NL         Philadelphia
#> 398   2015          Max Scherzer  30        Maj-NL           Washington
#> 399   2015    Anthony DeSclafani  25        Maj-NL           Cincinnati
#> 400   2015           John Axford  32        Maj-NL             Colorado
#> 401   2015          Matt Belisle  35        Maj-NL            St. Louis
#> 402   2015        Michael Blazek  26        Maj-NL            Milwaukee
#> 403   2015          Chris Martin  29        Maj-AL             New York
#> 404   2015      Franklin Morales  29        Maj-AL          Kansas City
#> 405   2015         Justin Wilson  27        Maj-AL             New York
#> 406   2015           Sergio Romo  32        Maj-NL        San Francisco
#> 407   2015       Dellin Betances  27        Maj-AL             New York
#> 408   2015        Jeurys Familia  25        Maj-NL             New York
#> 409   2015            Matt Grace  26        Maj-NL           Washington
#> 410   2015          Neil Ramírez  26        Maj-NL              Chicago
#> 411   2015        José Domínguez  24        Maj-AL            Tampa Bay
#> 412   2015         Huston Street  31        Maj-AL          Los Angeles
#> 413   2015       Aroldis Chapman  27        Maj-NL           Cincinnati
#> 414   2015          Joel Peralta  39        Maj-NL          Los Angeles
#> 415   2015           Zac Rosscup  27        Maj-NL              Chicago
#> 416   2015          Dan Jennings  28        Maj-AL              Chicago
#> 417   2015          Joakim Soria  31        Maj-AL              Detroit
#> 418   2015           Chris Young  36        Maj-AL          Kansas City
#> 419   2015     Carlos Villanueva  31        Maj-NL            St. Louis
#> 420   2015        Luke Gregerson  31        Maj-AL              Houston
#> 421   2015          Lucas Luetge  28        Maj-AL              Seattle
#> 422   2015             Tony Sipp  31        Maj-AL              Houston
#> 423   2015        Dallas Keuchel  27        Maj-AL              Houston
#> 424   2015         Andrew Miller  30        Maj-AL             New York
#> 425   2015            Yohan Pino  31        Maj-AL          Kansas City
#> 426   2015      Trevor Rosenthal  25        Maj-NL            St. Louis
#> 427   2015     Rafael Betancourt  40        Maj-NL             Colorado
#> 428   2015         Matt Thornton  38        Maj-NL           Washington
#> 429   2015       Brandon Cunniff  26        Maj-NL              Atlanta
#> 430   2015          Glen Perkins  32        Maj-AL            Minnesota
#> 431   2015         Adam Ottavino  29        Maj-NL             Colorado
#> 432   2015          Carson Smith  25        Maj-AL              Seattle
#> 433   2015           Sammy Solís  26        Maj-NL           Washington
#> 434   2015           Will Harris  30        Maj-AL              Houston
#> 435   2015       David Robertson  30        Maj-AL              Chicago
#> 436   2015           Yimi García  24        Maj-NL          Los Angeles
#> 437   2015           Pedro Strop  30        Maj-NL              Chicago
#> 438   2015          Kevin Jepsen  30        Maj-AL            Tampa Bay
#> 439   2015          Javier López  37        Maj-NL        San Francisco
#> 440   2015            Wade Davis  29        Maj-AL          Kansas City
#> 441   2015         Scott Carroll  30        Maj-AL              Chicago
#> 442   2015          Greg Holland  29        Maj-AL          Kansas City
#> 443   2015       Adam Liberatore  28        Maj-NL          Los Angeles
#> 444   2015          Brad Ziegler  35        Maj-NL              Arizona
#> 445   2015         Jerry Blevins  31        Maj-NL             New York
#> 446   2015        Enrique Burgos  24        Maj-NL              Arizona
#> 447   2015          Carter Capps  24        Maj-NL                Miami
#> 448   2015          Alex Claudio  23        Maj-AL                Texas
#> 449   2015      Carlos Contreras  24        Maj-NL           Cincinnati
#> 450   2015             Ike Davis  28        Maj-AL              Oakland
#> 451   2015         César Jiménez  30        Maj-NL         Philadelphia
#> 452   2015            Joe Nathan  40        Maj-AL              Detroit
#> 453   2015        Michael Tonkin  25        Maj-AL            Minnesota
#>      G GS  W  L SV   IP  H  R ER uBB BB SO HR HBP   ERA  AB X1B X2B X3B
#> 1    1  0 NA NA NA  1.1  6  6  6   1  1  3  1   0 40.50  10   2   2   1
#> 2    1  1 NA  1 NA  1.0  2  1  1   0  0  0  1   0  9.00   4   1   0   0
#> 3    1  0 NA NA NA  0.0  0  0  0   2  2  0  0   0    NA   0   0   0   0
#> 4    2  0 NA NA NA  2.0  6  6  6   2  2  1  1   0 27.00  11   1   4   0
#> 5    1  1 NA  1 NA  2.2 10  7  7   2  2  0  0   0 23.63  14   7   2   1
#> 6    1  0 NA NA NA  1.0  3  4  4   1  1  1  1   0 36.00   6   2   0   0
#> 7    5  0 NA NA NA  3.0  6  4  4   2  2  4  2   0 12.00  13   2   1   1
#> 8    1  0 NA NA NA  1.0  2  1  1   1  1  1  1   0  9.00   5   1   0   0
#> 9    1  0 NA NA NA  1.0  3  3  3   1  1  1  0   0 27.00   6   1   2   0
#> 10   1  1 NA  1 NA  1.2  6  6  6   2  2  2  0   0 32.40  10   4   2   0
#> 11   6  0  1 NA NA  5.0  9  7  5   1  1  6  4   0  9.00  24   3   2   0
#> 12   1  1 NA NA NA  2.1  7  3  3   1  1  3  1   0 11.57  14   5   1   0
#> 13   2  0 NA NA NA  1.0  2  0  0   0  0  1  0   0  0.00   4   1   1   0
#> 14   2  0 NA NA NA  1.1  3  3  3   1  1  0  0   0 20.25   5   2   1   0
#> 15   5  0 NA NA NA  2.2  6  4  4   1  1  2  1   0 13.50  14   3   2   0
#> 16   1  1 NA NA NA  2.0  9  9  4   0  1  1  0   0 18.00  14   7   2   0
#> 17   2  2 NA  1 NA  6.2 10  8  8   2  2  4  3   1 10.80  27   4   3   0
#> 18   1  1 NA  1 NA  5.0  8  3  3   1  1  5  3   0  5.40  23   4   1   0
#> 19   2  1 NA  1 NA  4.2  9  4  4   4  4  2  0   0  7.71  20   7   2   0
#> 20   2  0 NA NA NA  1.1  2  1  1   0  0  1  0   0  6.75   6   0   1   1
#> 21   1  1 NA NA NA  4.0  7  4  4   1  1  2  2   1  9.00  18   5   0   0
#> 22   4  4 NA  3 NA 17.2 30 20 17   9  9 10  6   1  8.66  82  18   4   2
#> 23   1  0 NA NA NA  1.0  2  1  1   1  1  1  0   0  9.00   5   1   1   0
#> 24   4  0 NA NA NA  5.0  8  7  5   2  2 11  3   1  9.00  23   5   0   0
#> 25   7  0 NA  2 NA  5.2 10  6  6   3  3  4  2   0  9.53  26   7   1   0
#> 26   6  0 NA  2 NA  5.1  9  7  6   4  5  6  1   0 10.13  25   4   2   2
#> 27   9  0 NA  1 NA  7.0 16 12 12   2  3  4  2   0 15.43  35  13   1   0
#> 28   8  0  1 NA NA  9.2 16 10  9   4  6  2  3   0  8.38  43   7   4   2
#> 29   1  1 NA  1 NA  3.0  6  6  6   3  4  2  0   0 18.00  15   3   2   1
#> 30   2  0 NA NA NA  1.1  3  1  1   1  1  2  0   0  6.75   7   2   1   0
#> 31   5  3 NA  2 NA 19.1 28 22 20  11 11 17  8   1  9.31  84  13   6   1
#> 32   2  2 NA  1 NA  7.0 13 11  9   3  3 10  1   0 11.57  34   6   5   1
#> 33   5  0 NA NA NA  3.0  8  2  2   1  2  4  0   0  6.00  16   7   1   0
#> 34  10  0 NA NA NA  8.0 11  8  8   7  7  5  3   0  9.00  34   7   1   0
#> 35   5  5  1  3 NA 28.0 39 26 26  10 11 18  8   2  8.36 116  20   8   3
#> 36   1  0 NA NA NA  1.0  3  1  1   0  0  2  0   0  9.00   6   3   0   0
#> 37   1  0 NA NA NA  2.1  4  1  1   1  2  2  0   1  3.86  10   2   2   0
#> 38   2  0  1 NA NA  2.0  2  1  1   1  1  1  1   1  4.50   8   1   0   0
#> 39   1  0 NA NA NA  2.0  2  1  1   1  1  2  1   1  4.50   8   1   0   0
#> 40   7  0  1  1  1  6.0 11  7  7   2  2  7  2   0 10.50  29   9   0   0
#> 41   5  2 NA  1 NA 11.1 19 17 16   7  7  9  1   1 12.71  51  13   5   0
#> 42   2  0 NA NA NA  2.0  2  0  0   2  2  1  0   0  0.00   7   1   0   1
#> 43   8  0 NA  2  4  7.0 13 10  9   6  7 11  0   1 11.57  32   9   4   0
#> 44  11  0  1 NA NA  6.1 10  4  4   3  3  7  2   1  5.68  29   8   0   0
#> 45   6  0 NA NA NA  9.2 16 13  6   3  3  8  3   1  5.59  45  10   3   0
#> 46   2  0 NA NA NA  4.1  5  8  8   5  5  4  1   1 16.62  18   3   1   0
#> 47   1  0  1 NA NA  5.0  6  2  2   3  3  1  1   0  3.60  19   3   2   0
#> 48   3  0 NA NA NA  5.1  9  3  3   2  2  3  1   0  5.06  24   7   1   0
#> 49   4  4 NA  3 NA 18.2 29 17 12   7  7 22  4   1  5.79  81  18   7   0
#> 50   4  4  1  2 NA 17.0 25 23 23   9  9 12  3   1 12.18  75  12  10   0
#> 51  11  0 NA  1 NA  9.2 14 11 10   9  9 12  1   1  9.31  43   9   4   0
#> 52   4  4  1  2 NA 16.1 24 17 15   9  9  7  3   3  8.27  70  18   3   0
#> 53   4  3 NA  1 NA 16.0 23 13 13   7  7 16  2   0  7.31  69  11   8   2
#> 54   4  4 NA  4 NA 13.0 21 19 19  11 12  7  1   2 13.15  58  17   3   0
#> 55   9  0  1  1 NA 10.0 13  7  7   3  3  6  3   0  6.30  39  10   0   0
#> 56   4  0 NA NA NA  4.1  7  2  2   0  0  5  1   0  4.15  20   4   2   0
#> 57   6  0 NA NA NA  6.0  8  9  8   4  5 12  2   0 12.00  26   5   1   0
#> 58   8  0 NA NA NA  9.0 14  6  6   4  5  6  1   0  6.00  38  10   3   0
#> 59   2  2 NA  1 NA 11.1 16  7  7   2  4  3  3   0  5.56  47   9   4   0
#> 60   4  4  2  2 NA 19.0 22 16 16  13 13 13  4   0  7.58  74  12   6   0
#> 61   2  2 NA  2 NA  7.2 12  8  8   4  4  7  1   0  9.39  34  10   0   1
#> 62  10  0 NA NA NA  6.0 11  5  5   0  2  6  0   0  7.50  27   6   5   0
#> 63   8  0 NA  2 NA  7.0 11 10  9   4  5  7  1   0 11.57  31   7   3   0
#> 64   4  4  3  1 NA 23.2 36 13 13   5  5 10  4   0  4.94 100  25   7   0
#> 65   7  0 NA  1 NA  3.1  7  2  1   2  3  3  0   0  2.70  16   7   0   0
#> 66   5  0 NA NA NA  4.0  6  5  5   4  4  4  0   1 11.25  18   4   2   0
#> 67   3  3 NA  1 NA 16.1 20 13 12   3  3 11  5   0  6.61  65   9   6   0
#> 68   5  5 NA  5 NA 24.2 32 24 24  14 15 12  2   2  8.76  99  20   9   1
#> 69   2  0 NA NA NA  3.0  4  3  3   0  0  0  1   0  9.00  13   2   1   0
#> 70   9  0 NA NA NA  2.2  5  3  2   1  1  2  0   0  6.75  13   4   1   0
#> 71   6  0 NA NA NA  5.2  7  4  2   4  4  5  1   1  3.18  24   5   1   0
#> 72   4  0 NA  1 NA  5.0  8  4  4   1  1  2  0   0  7.20  22   5   3   0
#> 73   4  4  1  1 NA 22.0 29 17 16   5  5 16  3   2  6.55  90  18   7   1
#> 74   5  5  2  1 NA 27.0 30 20 14  12 12 22  6   3  4.67 102  17   7   0
#> 75   5  5 NA  3 NA 21.0 28 17 16   9  9 17  2   0  6.86  86  16  10   0
#> 76   4  4  3 NA NA 23.0 24 15 15   4  4 29  9   0  5.87  90  12   3   0
#> 77  11  0 NA  1 NA  7.1 10  3  3   2  2  9  2   0  3.68  33   7   0   1
#> 78   6  0 NA NA  1  3.2  4  3  3   1  1  1  0   1  7.36  14   1   3   0
#> 79   9  0  1 NA  2  9.2  8  5  5   5  5  8  3   1  4.66  34   4   1   0
#> 80   7  0 NA NA NA  4.0  7  2  2   2  2  6  0   0  4.50  19   6   1   0
#> 81   3  0 NA  1 NA  3.1  6  3  2   0  1  3  0   0  5.40  16   3   3   0
#> 82   9  0  1 NA NA 11.1 13  6  6   5  5 14  1   0  4.76  43   6   6   0
#> 83   6  0 NA NA NA 10.1 10  9  8   6  7  5  3   2  6.97  40   5   2   0
#> 84   1  0 NA NA NA  3.0  5  2  2   0  2  0  1   0  6.00  14   3   1   0
#> 85   4  4  2  1 NA 21.0 25 14 14   5  5 17  4   1  6.00  83  13   8   0
#> 86   4  4  1 NA NA 21.2 31 10 10   5  5 20  2   1  4.15  90  23   6   0
#> 87   4  4  2  1 NA 19.2 26 14 13   8  8  9  3   0  5.95  83  18   5   0
#> 88   4  4  1  2 NA 19.2 19 12 11  13 14 16  4   0  5.03  73  10   5   0
#> 89   4  4  1  1 NA 23.0 25 15 15  10 10 10  4   1  5.87  87  15   6   0
#> 90   8  0 NA NA NA 12.0 14  7  7   4  5 10  2   0  5.25  49   6   4   2
#> 91   3  0 NA NA NA  4.1  6  4  4   2  3  7  0   0  8.31  18   4   1   1
#> 92   4  4  1  2 NA 23.1 29 14 13  10 11 22  1   1  5.01  93  19   5   4
#> 93   2  0 NA NA NA  2.2  5  2  2   0  1  3  0   0  6.75  13   3   2   0
#> 94   9  0  1 NA NA  8.1  9  9  9   2  2 11  3   2  9.72  34   5   1   0
#> 95   5  5  1  4 NA 29.2 34 24 24   5  5 19  8   1  7.28 118  19   7   0
#> 96  11  0 NA  2  4 10.0 12  5  4   3  4  8  2   0  3.60  39   7   3   0
#> 97   6  0 NA NA NA  4.1  3  3  3   4  4  0  1   1  6.23  15   1   1   0
#> 98   4  3 NA  3 NA 14.1 21 16 15   7  8  5  1   1  9.42  61  17   2   1
#> 99   7  0 NA  1  1  6.0 10  3  3   2  3  6  0   0  4.50  28   8   1   1
#> 100  9  0 NA NA NA  9.1 11  6  5   2  2  5  2   0  4.82  35   7   2   0
#> 101  4  4  1  3 NA 22.1 31 13 13   7  8 16  1   0  5.24  93  20   8   2
#> 102  4  4  2  1 NA 23.0 28 11  7   9  9 14  3   1  2.74  97  15   9   1
#> 103  4  4 NA  3 NA 25.0 32 14 14   5  6 12  3   0  5.04 102  19   8   2
#> 104  4  4  2  1 NA 22.0 24  8  8  12 13 13  2   1  3.27  83  14   8   0
#> 105  6  0  1 NA NA  5.2  4  3  3   3  3  5  2   0  4.76  20   1   1   0
#> 106 10  0  1  2  1  7.0  8  4  4   1  2  7  2   1  5.14  27   6   0   0
#> 107  4  4  1  2 NA 22.1 26 13 12  12 12  6  2   1  4.84  88  17   7   0
#> 108  5  2 NA  1  1 13.0 18  8  8   4  4  7  1   1  5.54  53  14   2   1
#> 109  4  4  1  2 NA 22.1 24 14 14   6  6 16  4   1  5.64  86  10   9   1
#> 110  8  0 NA  2 NA  8.0  9  8  8   2  2  9  3   0  9.00  33   6   0   0
#> 111  4  4  2  1 NA 23.0 28 15 14   6  7 24  3   1  5.48  90  17   8   0
#> 112  8  0 NA NA NA 11.0 16  5  5   2  3 11  1   0  4.09  47  12   3   0
#> 113  5  5  2  3 NA 29.1 33 16 15  13 13 19  5   1  4.60 119  22   6   0
#> 114  4  4  1  1 NA 19.2 26 12 12   4  4 11  2   0  5.49  82  19   4   1
#> 115  7  0 NA  2 NA 12.1 15  5  5   3  3  0  1   0  3.65  46  10   4   0
#> 116  4  0 NA  1 NA  9.0 11  7  6   3  4  3  1   0  6.00  36   6   3   1
#> 117  5  5  1  1 NA 23.1 23 11 10  12 12 18  3   2  3.86  86  13   7   0
#> 118  4  4  1  2 NA 19.2 25 16 15  10 11 19  1   2  6.86  81  19   5   0
#> 119  3  0 NA NA NA  1.2  2  0  0   1  1  0  0   0  0.00   6   2   0   0
#> 120  4  4  1  2 NA 15.2 17 16 15  11 11 10  1   0  8.62  62  11   4   1
#> 121  4  4 NA  2 NA 21.2 29 15 15   5  5 24  1   0  6.23  90  20   7   1
#> 122  5  5 NA  3 NA 29.1 35 19 19   4  4 13  6   1  5.83 121  23   5   1
#> 123  7  0 NA  1 NA  8.2 12  8  7   2  2  2  1   0  7.27  37   8   3   0
#> 124  9  0 NA NA NA  8.1 10  4  3   2  2 10  1   0  3.24  35   6   2   1
#> 125  7  1 NA  1 NA 15.2 25  8  8   3  5 15  1   0  4.60  69  22   2   0
#> 126 11  0  2 NA  6  9.2 10  3  3   4  4  8  1   2  2.79  36   8   0   1
#> 127  1  1 NA  1 NA  5.0  5  3  3   2  2  3  0   1  5.40  18   2   3   0
#> 128  2  2 NA  1 NA  9.1 10  5  5   7  8  8  1   1  4.82  37   7   2   0
#> 129  4  4  2  1 NA 23.2 31 12 10   4  5 15  4   0  3.80 100  23   4   0
#> 130  9  0 NA  1 NA  5.1  9  1  1   2  2  5  0   0  1.69  24   9   0   0
#> 131  5  5 NA  4 NA 31.2 37 16 16   2  2 26  6   0  4.55 128  20  10   1
#> 132  4  0 NA  1 NA  7.1  8  5  5   4  4  5  1   0  6.14  27   5   2   0
#> 133  9  0 NA  1 NA 11.0 14  5  5   5  6  8  0   0  4.09  45  10   3   1
#> 134 11  0  1  1 NA 12.1 14  6  5   2  3  6  1   0  3.65  47   7   5   1
#> 135 11  0 NA  1 NA 10.0 13  4  3   4  4  8  1   0  2.70  42  11   1   0
#> 136  3  0 NA  1 NA  3.1  5  4  4   1  1  2  0   0 10.80  15   4   1   0
#> 137  5  5  2 NA NA 27.0 30 21 20  11 11 21  4   3  6.67 110  21   5   0
#> 138 12  0  1  2  1 11.1 14  6  5   4  4 10  2   0  3.97  45  12   0   0
#> 139  5  5  1  2 NA 32.0 38 18 17   6  6 22  4   2  4.78 131  23  10   1
#> 140  9  0 NA NA NA  8.2 10  6  6   1  1 10  2   0  6.23  35   7   1   0
#> 141  8  0 NA  1  2  7.1  7  8  8   4  4  7  1   0  9.82  29   3   3   0
#> 142 10  0 NA  1  6  8.2  9  5  5   4  4 10  1   0  5.19  33   7   0   1
#> 143  7  0  1 NA NA 11.0 11  7  6   5  5 11  2   0  4.91  43   6   3   0
#> 144  4  4  2  1 NA 22.2 22 12 12  11 11 13  5   0  4.76  86  15   2   0
#> 145  4  4 NA  4 NA 25.2 31 18 17   3  4 22  4   1  5.96  99  25   0   2
#> 146  4  4  2  1 NA 22.0 27 14 13   5  5 20  2   1  5.32  89  20   5   0
#> 147  9  0 NA  1 NA  8.2 10  5  5   2  2  9  1   2  5.19  35   8   1   0
#> 148  2  0 NA NA NA  2.2  3  0  0   1  1  2  0   1  0.00  10   3   0   0
#> 149  9  0 NA  1 NA  8.2  8  6  6   3  3  9  1   1  6.23  31   5   1   1
#> 150  4  4  2 NA NA 20.0 18  6  6  13 13 13  1   3  2.70  70  14   2   1
#> 151  5  0  1  1 NA  7.2  6  2  2   5  5  9  1   2  2.35  28   4   1   0
#> 152  2  0 NA NA NA  2.0  3  1  1   0  0  2  0   0  4.50   8   2   1   0
#> 153 11  0 NA  1  5 10.1 11  6  6   3  4  7  1   0  5.23  38   7   3   0
#> 154  9  0  1  2 NA 10.2 13  6  5   5  6  9  0   1  4.22  44   9   4   0
#> 155  5  5 NA  3 NA 31.0 29 19 18  13 13 20  5   3  5.23 114  19   4   1
#> 156  4  4  1  2 NA 24.0 24 10 10   6  6 15  3   0  3.75  89  15   3   3
#> 157  5  5  1  2 NA 27.2 27 14 14  17 18 37  3   1  4.55 109  17   6   1
#> 158  4  0 NA NA NA  3.1  2  2  1   3  3  3  1   0  2.70  11   1   0   0
#> 159  7  0 NA NA NA  9.0 11  7  6   5  5  4  0   0  6.00  36   7   4   0
#> 160  6  0 NA NA NA  4.2  5  4  4   2  2  3  1   0  7.71  19   4   0   0
#> 161  5  5  2  2 NA 29.1 38 18 15   8  8 30  1   2  4.60 123  30   7   0
#> 162 10  0  1 NA  1 10.0 10  4  3   6  6  8  0   0  2.70  40   4   6   0
#> 163  6  0 NA NA NA  3.2  3  2  2   2  3  5  1   1  4.91  14   2   0   0
#> 164  1  1 NA NA NA  5.0  5  3  3   2  2  4  0   0  5.40  18   2   3   0
#> 165  7  0  1  2 NA  8.0  7  4  3   6  7  4  1   0  3.38  27   5   1   0
#> 166  9  0 NA  1 NA  7.0  7  3  3   3  3  3  1   0  3.86  26   4   2   0
#> 167  4  4  2  2 NA 24.0 28 13 12   7  9 18  1   0  4.50  96  16  11   0
#> 168 10  0 NA  1 NA 10.1 12  6  6   1  2 10  1   1  5.23  41   6   4   1
#> 169  9  0  1 NA NA  6.1  7  4  3   3  4  4  1   0  4.26  25   5   1   0
#> 170  1  1 NA NA NA  5.2  6  2  2   3  3  6  0   1  3.18  22   5   1   0
#> 171  5  5  2  2 NA 27.1 23 15 15  10 10 20  4   3  4.94 100  12   7   0
#> 172  5  5  1  1 NA 29.0 32 13 13  11 11 20  1   0  4.03 111  25   4   2
#> 173  5  5  1  4 NA 31.2 32 17  9   8  8 36  5   2  2.56 123  21   6   0
#> 174  4  4  2  1 NA 25.0 27 14 13   4  5 25  4   0  4.68 100  15   8   0
#> 175  5  5  2  2 NA 29.2 29 17 14  16 16 15  1   1  4.25 106  22   6   0
#> 176 10  0  1 NA NA  8.2  8  5  5   0  0 10  3   0  5.19  33   4   1   0
#> 177  1  1  1 NA NA  5.0  4  2  2   1  1  5  1   0  3.60  18   1   2   0
#> 178  5  5  1  3 NA 25.0 31 18 16   8  8 33  2   1  5.76 105  25   4   0
#> 179  5  5  2  2 NA 32.0 30 20 19  10 10 29  6   2  5.34 124  19   4   1
#> 180  5  5  2  2 NA 31.1 34 16 15   5  5 16  5   0  4.31 122  26   2   1
#> 181  5  5  1  3 NA 29.2 30 18 18   8  8 31  5   0  5.46 115  18   7   0
#> 182  1  0 NA NA NA  2.0  2  3  0   2  2  1  0   0  0.00   9   1   1   0
#> 183  5  5  2 NA NA 28.2 31 12 11   9  9 23  2   0  3.45 110  23   5   1
#> 184  8  0  1  1  2  9.0 11  5  5   4  5  9  1   0  5.00  39   9   1   0
#> 185  4  4  1  1 NA 24.2 27 12  9   8  9 11  3   0  3.28  94  20   3   1
#> 186  4  4 NA  2 NA 25.1 27 11 11   6  7 13  3   2  3.91  96  21   3   0
#> 187  5  5 NA  2 NA 26.2 30 20 17   8  8 26  4   0  5.74 112  21   5   0
#> 188  3  3  3 NA NA 19.0 18  7  7   5  5 28  3   2  3.32  73  12   3   0
#> 189  5  5  2  3 NA 26.2 29 14 12   8  8 26  3   0  4.05 108  21   4   1
#> 190  4  4  2  2 NA 22.0 22 11 11   8  8 12  3   0  4.50  84  17   2   0
#> 191  4  4  2  1 NA 22.2 26 13 12   6  7 20  0   1  4.76  89  18   7   1
#> 192  2  0 NA NA NA  3.1  3  2  2   3  3  2  0   0  5.40  11   2   1   0
#> 193  4  4  2  2 NA 26.0 26 11  8   6  6 20  2   1  2.77 100  14  10   0
#> 194  8  0 NA  1 NA  8.1  8  6  6   2  2  5  1   1  6.48  31   5   2   0
#> 195  5  5  1  2 NA 31.1 32 16 13   7  7 43  4   1  3.73 120  22   6   0
#> 196  4  4  1  2 NA 22.1 23 10  9   6  7 26  1   1  3.63  86  13   7   2
#> 197 11  0  3 NA NA 10.2  9  2  2   7  7  9  0   0  1.69  37   6   3   0
#> 198  4  4  1  2 NA 22.0 22 14 11   5  5 21  3   0  4.50  87  13   5   1
#> 199 10  0 NA NA NA 10.2 13  6  5   3  3  8  0   0  4.22  44   9   4   0
#> 200  4  4  1  2 NA 22.0 23  9  8   8 10 16  1   0  3.27  79  18   3   1
#> 201  4  4 NA  1 NA 23.1 24 11 11   5  6 20  2   0  4.24  85  16   6   0
#> 202  2  2 NA  2 NA 10.1  8  6  6   6  7 11  2   1  5.23  39   5   1   0
#> 203  9  0  1  1 NA  9.1 11  5  5   3  9  7  0   1  4.82  34   7   3   1
#> 204  4  4 NA  1 NA 25.1 27 12 12   2  3 15  3   0  4.26  97  18   4   2
#> 205  4  4  2 NA NA 22.2 21 13 13   9  9 20  1   3  5.16  87  13   6   1
#> 206  2  2 NA  1 NA 11.0 10  6  6   2  3  7  3   2  4.91  43   7   0   0
#> 207  4  4  1  1 NA 22.1 20 10 10   6  6 13  3   0  4.03  81  11   5   1
#> 208  2  0 NA NA NA  3.0  3  0  0   2  2  2  0   0  0.00  11   3   0   0
#> 209  1  1 NA  1 NA  7.0  4  3  3   2  2  6  2   0  3.86  24   0   2   0
#> 210  5  0 NA  1 NA 11.0 12  5  5   2  2  7  0   0  4.09  43   7   4   1
#> 211  5  5  1  1 NA 28.1 19 16 15  16 16 25  5   2  4.76  99  10   3   1
#> 212 11  0  1  1 NA 11.0  9  7  6   1  1  9  2   2  4.91  41   4   3   0
#> 213  8  1  1  1 NA 12.2 11  6  6  11 12  9  1   0  4.26  49   9   1   0
#> 214  4  4  1  1 NA 20.2 21 11 10   8  8 12  2   0  4.35  80  17   2   0
#> 215  5  5  2  2 NA 27.2 32 19 15   6  6 18  1   2  4.88 109  28   3   0
#> 216  4  4  2  1 NA 23.2 20  9  9  11 11 21  3   1  3.42  88  14   3   0
#> 217  2  0 NA NA NA  1.2  2  2  2   1  1  4  0   0 10.80   7   2   0   0
#> 218  1  1  1 NA NA  5.0  6  1  1   1  1  5  0   0  1.80  21   4   2   0
#> 219  9  0 NA  1 NA 11.0  7  7  6   3  3 10  3   4  4.91  41   4   0   0
#> 220  4  4 NA  1 NA 20.2 21 12 12   4  4 19  2   1  5.23  82  14   4   1
#> 221  2  0 NA NA NA  1.2  2  1  1   0  0  0  0   0  5.40   6   1   1   0
#> 222 12  0 NA NA NA 10.2  9  5  4   3  5  6  2   2  3.38  41   4   3   0
#> 223  5  5  2  1 NA 31.1 33 13 13   5  5 26  3   3  3.73 124  26   3   1
#> 224  4  4  2  2 NA 15.2 18  8  8   1  3 23  1   1  4.60  62  13   3   1
#> 225  5  5  2  3 NA 29.2 32 13 11   6  7 23  4   0  3.34 115  25   3   0
#> 226  5  5  2 NA NA 31.0 26 13 10   8  9 41  4   2  2.90 117  11  10   1
#> 227  6  0 NA NA NA  5.1  6  1  1   0  0  3  0   0  1.69  20   4   2   0
#> 228 11  0  1  1  7 10.1 11  6  6   5  6 11  0   0  5.23  40   9   2   0
#> 229 12  0  3  1  1 11.0  8  5  5   5  5 11  2   0  4.09  38   5   1   0
#> 230  7  0  1 NA NA 12.1 12  7  7   4  4 11  1   0  5.11  48   8   3   0
#> 231 10  0 NA NA NA 12.2 12  8  5   7  9 11  1   0  3.55  49   7   4   0
#> 232 11  0  1 NA NA 12.2 10  4  4   4  4 16  2   1  2.84  44   7   0   1
#> 233 10  0 NA  1 NA 12.1 12  6  6   3  4 11  1   0  4.38  46   7   4   0
#> 234 10  0 NA NA NA  8.1  6  2  2   5  5 11  2   0  2.16  32   4   0   0
#> 235  4  4  1  2 NA 23.2 20 14 10   6  7 16  4   0  3.80  89  11   3   2
#> 236  5  5  3  1 NA 31.1 30 17 16   7  7 20  2   1  4.60 114  22   5   1
#> 237  5  1  1 NA NA 12.1 12  1  1   3  3  7  0   0  0.73  46   7   5   0
#> 238  4  4  2  1 NA 20.1 25 10 10   2  3 17  1   0  4.43  84  20   4   0
#> 239  5  5  3 NA NA 31.1 33 13 13   2  2 32  2   1  3.73 121  21   9   1
#> 240  9  0 NA NA NA  9.0 11  4  3   2  3  6  0   0  3.00  36   9   2   0
#> 241  5  5 NA  3 NA 34.0 36 18 16   5  7 36  2   2  4.24 130  27   7   0
#> 242  5  5  1  2 NA 31.0 21 12 11  14 16 32  7   0  3.19 112  11   3   0
#> 243  2  0 NA NA NA  4.0  2  2  2   2  2  4  0   1  4.50  13   0   1   1
#> 244  5  5  2  3 NA 32.2 34 12 10   3  3 16  1   0  2.76 124  22  10   1
#> 245  5  5  4  1 NA 31.2 31 11 11   6  6 21  2   1  3.13 118  23   6   0
#> 246  2  2 NA  2 NA 12.0 13  6  6   1  1  5  1   0  4.50  47   9   3   0
#> 247  4  0 NA NA NA  4.2  5  3  3   2  2  4  0   0  5.79  17   5   0   0
#> 248 10  0  1  1 NA 11.1 11  5  5   0  1  8  1   0  3.97  42   6   3   1
#> 249  9  0  1 NA NA 10.0  8  5  5   4  4  6  1   0  4.50  37   4   3   0
#> 250  3  0 NA NA NA  3.2  4  3  2   2  2  3  0   0  4.91  14   4   0   0
#> 251  4  4 NA  1 NA 22.2 15 12  7   9  9 16  4   3  2.78  79  10   0   1
#> 252  9  0 NA NA NA  8.2  7  4  4   2  3  9  2   0  4.15  31   4   1   0
#> 253  8  0 NA NA NA  6.0  4  2  2   3  3  5  1   0  3.00  22   2   1   0
#> 254  4  0  1 NA NA 10.0  9  5  4   3  3 14  1   0  3.60  39   6   2   0
#> 255 10  0  1 NA NA  9.1  9  6  0   6  6 10  0   0  0.00  39   7   2   0
#> 256 11  0 NA  2  1  8.1  7  9  7   3  3 15  0   2  7.56  32   4   3   0
#> 257  3  3  2  1 NA 18.0 17  7  7   8  8 10  0   0  3.50  68  14   3   0
#> 258 11  0  1  1 NA  8.0  7  4  4   2  2  7  1   0  4.50  30   5   1   0
#> 259 12  0 NA NA NA 11.0 11  4  3   2  2 12  0   0  2.45  41   9   1   1
#> 260  4  4  2 NA NA 24.0 18  7  6   9  9 15  2   3  2.25  86  13   3   0
#> 261  9  0  1 NA  1  7.0  8  5  5   0  0  6  0   0  6.43  27   5   3   0
#> 262  4  4  1  1 NA 25.2 26 12 12   5  5 14  2   0  4.21 100  20   4   0
#> 263  7  0 NA  2 NA  8.1  7  5  4   5  6  4  0   0  4.32  32   4   3   0
#> 264  9  0  1 NA NA  5.2  5  1  1   2  2  5  0   1  1.59  20   5   0   0
#> 265  2  2 NA NA NA 10.2  8  4  4   1  1 15  3   0  3.38  38   5   0   0
#> 266  4  4  2  1 NA 23.2 17  7  6  11 12 22  3   2  2.28  87  11   3   0
#> 267  6  0 NA NA NA  7.1  4  2  2   6  6  7  0   0  2.45  24   2   2   0
#> 268  5  5  4  1 NA 32.2 31 12 12   1  1 25  4   0  3.31 124  19   7   1
#> 269  5  5  2  1 NA 31.0 29 15 12   9 10 29  1   1  3.48 119  21   5   2
#> 270  3  0 NA NA NA  5.1  5  3  3   0  0  3  1   0  5.06  20   4   0   0
#> 271  4  4  1 NA NA 27.0 22  8  8   6  6 19  2   0  2.67  93  16   2   2
#> 272 11  0  1  1 NA  9.2 11  3  3   4  6  9  0   0  2.79  39  11   0   0
#> 273  4  4  1 NA NA 23.2 18 13 13   8  8 28  3   1  4.94  87  12   3   0
#> 274  9  0 NA  2  1  9.1  7  3  3   5  6  7  1   0  2.89  36   4   2   0
#> 275  5  0 NA NA  1  9.1  7  3  3   3  3  7  0   2  2.89  32   5   2   0
#> 276  4  4  2  1 NA 27.1 26  7  7   3  3 18  2   0  2.30 104  18   6   0
#> 277  4  4  3 NA NA 24.2 25  8  8   4  4 23  0   0  2.92  96  19   5   1
#> 278  7  0  2  1  4  6.1  5  2  2   0  0 11  1   0  2.84  22   3   0   1
#> 279  4  4 NA  1 NA 25.0 24  5  5   6  7 20  1   1  1.80  93  22   1   0
#> 280  4  4  2  1 NA 25.1 25 11 10   1  1 23  3   1  3.55  98  19   3   0
#> 281  7  0 NA  1 NA  6.2  7  1  0   2  3  7  0   0  0.00  26   6   1   0
#> 282  4  4  2  1 NA 24.0 14  9  9   8  8 17  6   0  3.38  83   8   0   0
#> 283 10  0 NA NA NA 10.0  8  3  3   3  4  8  0   1  2.70  34   6   2   0
#> 284 10  0 NA  1 NA 11.2 10  5  5   4  4  7  0   0  3.86  44   5   5   0
#> 285 12  0  1 NA NA  9.0  7  6  6   1  2  6  2   1  6.00  33   5   0   0
#> 286  3  0 NA NA NA  3.1  3  0  0   2  2  2  0   0  0.00  13   3   0   0
#> 287  5  5  3  1 NA 29.0 21  8  7  10 12 23  3   1  2.17 105  12   6   0
#> 288  8  0 NA NA NA  4.1  5  2  2   1  1  5  0   0  4.15  17   5   0   0
#> 289  4  0 NA NA NA  5.2  4  1  1   1  1  5  1   0  1.59  22   1   2   0
#> 290  9  0 NA NA NA 13.0  9  3  2   6  7 12  1   1  1.38  45   6   2   0
#> 291  4  4  1  2 NA 22.1 16 11 10   8  9 18  1   2  4.03  74  14   1   0
#> 292  8  0  1  1 NA  9.1  8  2  2   2  3  8  0   0  1.93  32   5   3   0
#> 293  1  0 NA NA NA  2.1  3  0  0   0  0  2  0   0  0.00  10   3   0   0
#> 294  4  4  2  1 NA 23.2 19  8  8   5  6 26  3   0  3.04  88  13   3   0
#> 295 10  0 NA NA  1  7.2  8  3  3   0  0  6  0   1  3.52  30   7   1   0
#> 296  8  0 NA  1  7  8.0  5  4  4   3  3 13  1   0  4.50  29   2   2   0
#> 297 10  0  1 NA  4 10.0  9  2  2   1  1  9  0   0  1.80  39   4   5   0
#> 298  2  0 NA NA NA  4.2  2  1  1   2  2  1  1   0  1.93  16   0   1   0
#> 299  9  0  1  2 NA 10.1 10  5  5   0  0  3  1   0  4.35  40   8   0   1
#> 300  6  0 NA NA NA  5.2  5  1  1   2  3  4  0   0  1.59  22   3   2   0
#> 301  5  4  3 NA NA 26.2 17  5  5   9 10 24  4   0  1.69  92  11   2   0
#> 302  3  3  2  1 NA 18.0 12  7  6  10 10 14  1   0  3.00  62  10   1   0
#> 303  7  0  1  1 NA 10.0  6  3  3   4  5  9  1   1  2.70  34   4   0   1
#> 304  9  0  1  1 NA  8.1  7  2  2   4  4 10  0   0  2.16  31   6   1   0
#> 305  4  4  4 NA NA 26.2 22  9  9   3  3 31  3   2  3.04 100  16   3   0
#> 306  2  0 NA  1 NA  1.1  0  1  1   3  4  1  0   0  6.75   3   0   0   0
#> 307  4  4  2  1 NA 25.0 23  6  4   3  3 18  0   0  1.44  93  15   8   0
#> 308  1  0 NA NA NA  5.0  3  1  1   1  3  4  1   0  1.80  16   1   1   0
#> 309  9  0 NA NA NA 10.1  8  2  2   2  3  3  1   0  1.74  36   6   1   0
#> 310  9  0 NA  1 NA  8.0  6  5  5   1  1  6  1   0  5.63  29   3   2   0
#> 311  4  4  1  1 NA 25.1 16  7  7   7  7 12  2   0  2.49  87   8   4   2
#> 312  4  4  1  1 NA 22.0 17 10  7   4  4 11  1   3  2.86  79  13   3   0
#> 313  8  0  2 NA NA  8.2  5  6  5   7  7  9  0   0  5.19  28   3   2   0
#> 314  7  0 NA NA NA 18.1 15  7  7   4  4 13  1   0  3.44  67  11   3   0
#> 315  7  0 NA  1 NA  9.1  7  2  2   6  7 12  0   0  1.93  35   6   1   0
#> 316  4  4  4 NA NA 26.0 21  7  7   6  6 16  2   1  2.42  96  15   4   0
#> 317  4  4  1  2 NA 26.0 21 10  9   8  9 18  1   1  3.12  98  16   3   1
#> 318  4  4  2 NA NA 26.0 21  2  1   8  8 11  0   2  0.35  94  18   3   0
#> 319  9  0 NA NA  5  9.0  8  2  2   3  3 11  0   0  2.00  33   8   0   0
#> 320 11  0 NA NA NA 10.2  8  2  2   1  1 11  2   0  1.69  39   6   0   0
#> 321  1  1 NA NA NA  5.2  5  1  1   2  2  5  0   0  1.59  21   5   0   0
#> 322  9  0  1  1 NA 10.2  7  2  2   3  3  7  1   0  1.69  36   5   1   0
#> 323  8  0 NA NA NA  7.2  7  5  4   2  2 10  0   0  4.70  28   7   0   0
#> 324 11  0  1 NA  1  9.2  7  2  2   3  3 12  0   1  1.86  34   5   1   1
#> 325  6  2  2 NA NA 21.1 12  7  7   7  7 10  2   2  2.95  75   6   3   1
#> 326  3  0 NA NA NA  2.1  0  0  0   4  4  3  0   0  0.00   7   0   0   0
#> 327  7  0 NA  1 NA  6.1  6  5  4   0  1  7  1   0  5.68  25   5   0   0
#> 328  7  0 NA  2  3  7.0  4  2  2   2  2  6  1   0  2.57  24   2   1   0
#> 329  4  4  2 NA NA 25.0 15  5  5  13 13 28  1   0  1.80  86  11   3   0
#> 330 11  0 NA NA NA  9.0  6  0  0   4  4  8  0   0  0.00  29   5   1   0
#> 331 10  0  1 NA NA 10.0  8  5  5   1  3  6  1   1  4.50  37   5   2   0
#> 332 10  0 NA  1  2  8.1  5  3  3   2  2 11  1   2  3.24  29   4   0   0
#> 333  6  0 NA  1 NA 15.1 11  5  4   3  3 16  2   1  2.35  55   9   0   0
#> 334  6  2 NA  2  1 17.2 13  6  5   6  7 18  1   1  2.55  63  10   2   0
#> 335  7  0 NA NA NA  6.0  4  0  0   2  2  5  0   0  0.00  21   2   2   0
#> 336  2  0  1 NA NA  2.1  2  0  0   1  1  2  0   0  0.00   9   2   0   0
#> 337  9  0  1 NA NA  9.1  5  3  3   3  3  9  1   0  2.89  33   2   1   1
#> 338  5  1 NA  1 NA 10.0  9  6  5   2  5 13  0   0  4.50  40   5   3   1
#> 339  5  5  2  2 NA 32.1 24  9  9   7  7 27  1   0  2.51 117  17   4   2
#> 340  5  3  1 NA NA 18.2 15  7  7   7  7 12  0   0  3.38  70  13   2   0
#> 341  5  5  4 NA NA 32.2 23  7  7   7  7 27  2   1  1.93 119  15   5   1
#> 342  5  5  1  1 NA 35.2 22 12 12   7 10 25  5   0  3.03 123  11   6   0
#> 343  2  0 NA NA NA  6.1  2  3  1   3  3  6  1   2  1.42  22   1   0   0
#> 344 10  1  1 NA  1 11.2  6  4  4   5  5 14  1   1  3.09  40   4   1   0
#> 345  6  0 NA NA NA  5.0  5  3  3   0  0  5  0   0  5.40  19   4   1   0
#> 346  4  4  2  1 NA 22.1 14 10  8   7  7 24  2   0  3.22  80   7   5   0
#> 347  6  0  1  1 NA  8.2  5  2  2   2  2  8  1   1  2.08  31   3   1   0
#> 348  5  0 NA NA NA  8.0  6  1  1   3  3  5  0   0  1.13  27   6   0   0
#> 349 11  0  1 NA NA 11.2  9  3  3   4  4  6  0   0  2.31  43   8   1   0
#> 350  9  0 NA NA  4  9.1  7  2  2   2  2 13  0   0  1.93  35   4   3   0
#> 351 11  0 NA  1 NA 10.2  7  5  4   3  3 13  0   1  3.38  39   4   3   0
#> 352 11  0 NA NA NA 10.0  7  5  5   3  3  5  0   0  4.50  37   5   1   1
#> 353 10  0  2  1  5  9.1  5  2  2   5  5 13  0   0  1.93  32   3   2   0
#> 354  3  0 NA NA NA  4.1  3  2  2   1  1  2  0   0  4.15  14   2   1   0
#> 355  8  0 NA NA NA  8.2  7  4  3   1  1  8  0   1  3.12  33   5   2   0
#> 356 10  0  1 NA NA 13.2  9  3  3   6  7 13  0   0  1.98  46   8   1   0
#> 357  3  0 NA NA NA  2.1  0  0  0   3  3  4  0   0  0.00   6   0   0   0
#> 358 10  0 NA NA NA 13.0  9  2  2   3  4 15  0   0  1.38  45   6   2   1
#> 359  5  5  4 NA NA 30.2 22  8  6   8  8 35  1   1  1.76 111  19   2   0
#> 360 11  0  1 NA NA 15.2 11  3  3   3  3  8  1   0  1.72  56   8   2   0
#> 361 10  0 NA NA NA 13.0  8  2  2   1  1 14  2   0  1.38  45   5   1   0
#> 362  1  1  1 NA NA  6.0  5  0  0   1  1  5  0   0  0.00  23   4   1   0
#> 363 12  0  1  1  1 13.0 10  4  4   0  0 13  2   0  2.77  49   8   0   0
#> 364 10  0 NA NA  1  7.1  3  3  3   4  4 10  1   0  3.68  25   2   0   0
#> 365  9  0 NA  1 NA  6.1  4  1  1   2  3  2  0   0  1.42  22   2   2   0
#> 366  4  4  3  1 NA 26.2 18  6  6   7  7 25  1   0  2.03  98  12   5   0
#> 367  6  0  1 NA NA 10.2  4  1  1   5  5 11  1   1  0.84  35   2   1   0
#> 368  5  5  4 NA NA 34.2 22  7  7   6  6 36  1   3  1.82 121  15   5   1
#> 369 10  0 NA NA NA 10.0  9  1  1   1  2 11  0   0  0.90  38   8   1   0
#> 370 10  0 NA NA NA  5.1  3  1  1   3  3  5  0   0  1.69  18   3   0   0
#> 371  1  0 NA NA NA  2.0  1  0  0   1  1  0  0   0  0.00   6   1   0   0
#> 372  4  4  1  1 NA 24.1 11  6  6  12 12 30  2   1  2.22  83   8   1   0
#> 373 12  0  2 NA NA 10.1  7  2  2   2  2 15  0   0  1.74  37   4   3   0
#> 374  1  0 NA NA NA  2.0  2  0  0   0  0  1  0   0  0.00   8   2   0   0
#> 375  4  4  2 NA NA 20.0  9  4  4  11 11 14  0   1  1.80  65   7   2   0
#> 376 11  0 NA NA NA 12.2  7  2  2   4  4 18  0   1  1.42  43   4   3   0
#> 377 11  0 NA NA NA 14.0  7  4  4   4  5  7  2   0  2.57  46   4   1   0
#> 378  5  5  3 NA NA 36.1 26  9  8   6  6 25  1   1  1.98 129  23   2   0
#> 379 10  0 NA NA NA  7.1  3  2  2   4  5  9  0   0  2.45  25   0   3   0
#> 380  9  0 NA NA NA 11.0  6  2  2   2  3 13  1   0  1.64  36   3   2   0
#> 381 12  0 NA  1  1 10.1  7  1  1   3  4 12  0   0  0.87  37   5   2   0
#> 382  1  0 NA NA NA  3.0  2  0  0   0  0  0  0   0  0.00   9   1   1   0
#> 383  5  5  2  2 NA 33.2 21  9  9   8  8 26  0   1  2.41 120  14   6   1
#> 384 11  0 NA  1 NA  5.2  5  3  2   1  1  5  0   0  3.18  23   5   0   0
#> 385  8  0 NA NA NA  8.1  4  3  1   1  1  7  2   0  1.08  30   2   0   0
#> 386 10  0 NA NA NA  5.1  5  6  4   1  3  8  0   0  6.75  21   5   0   0
#> 387  5  5  2  2 NA 37.0 22  9  8   4  5 38  3   2  1.95 128  14   5   0
#> 388  9  0 NA  1 NA  8.1  5  1  1   4  5 10  0   0  1.08  29   5   0   0
#> 389  4  4  2  2 NA 28.1 19 10  6   5  5 23  1   1  1.91 102  16   2   0
#> 390  4  4  2 NA NA 27.1 15  3  3   9  9 30  1   1  0.99  95  12   2   0
#> 391  4  4  2  1 NA 22.2 10  6  4   8  8 22  2   1  1.59  75   8   0   0
#> 392  5  0 NA NA NA  4.0  2  0  0   2  3  3  0   0  0.00  13   1   1   0
#> 393  9  0  1 NA NA  9.1  7  2  2   1  1 13  0   0  1.93  34   6   1   0
#> 394  5  5  3  2 NA 32.1 18  5  3   6  6 37  2   2  0.84 113  13   3   0
#> 395  5  0  1 NA NA  7.0  6  1  0   1  1  6  0   0  0.00  28   6   0   0
#> 396  9  0 NA  1 NA  9.0  5  3  3   2  3  5  1   0  3.00  30   4   0   0
#> 397  8  0 NA NA  5  8.1  4  1  1   1  1  9  1   1  1.08  29   2   1   0
#> 398  4  4  1  2 NA 28.2 20  7  4   4  4 29  0   1  1.26 108  17   1   2
#> 399  4  4  2  1 NA 26.0 14  7  3   5  6 21  2   1  1.04  91   9   3   0
#> 400  5  0 NA NA  3  5.0  3  0  0   2  2  6  0   0  0.00  18   3   0   0
#> 401  9  0  1 NA NA  8.2  5  1  1   3  3  7  0   0  1.04  30   4   1   0
#> 402  9  0  1 NA NA 11.0  6  2  1   4  4  9  0   0  0.82  38   5   1   0
#> 403 12  0 NA NA  1 11.0  7  3  3   2  2 13  0   1  2.45  42   5   2   0
#> 404 10  0  2 NA NA  9.0  5  2  2   1  1  6  0   0  2.00  31   2   2   1
#> 405 11  0  1 NA NA  7.0  2  3  3   5  5  7  0   0  3.86  23   1   1   0
#> 406  9  0 NA  1 NA  7.1  4  2  2   2  3 11  0   0  2.45  26   2   2   0
#> 407 11  0  3 NA NA 12.1  5  1  0   7  7 19  0   0  0.00  42   5   0   0
#> 408 11  0 NA NA  9 10.2  4  2  2   3  3 13  1   0  1.69  34   2   1   0
#> 409  5  0 NA NA NA  3.1  2  0  0   1  1  3  0   0  0.00  12   2   0   0
#> 410  4  0  1 NA NA  3.0  1  1  1   2  2  4  0   0  3.00  10   1   0   0
#> 411  4  0  1 NA NA  5.2  2  0  0   2  2  5  0   0  0.00  16   1   1   0
#> 412  9  0 NA NA  9  9.0  5  1  1   3  3 10  0   0  1.00  32   5   0   0
#> 413 11  0  1 NA  5 10.2  5  0  0   3  3 19  0   1  0.00  37   4   1   0
#> 414  7  0  1 NA  3  5.2  2  0  0   3  3  4  0   0  0.00  18   2   0   0
#> 415  8  0  1 NA NA  8.2  3  1  1   1  1 11  1   0  1.04  30   0   1   1
#> 416  9  0 NA NA NA 10.0  5  3  3   3  6 10  0   0  2.70  34   3   2   0
#> 417 12  0  1 NA  9 10.2  5  2  2   2  2  7  1   0  1.69  37   4   0   0
#> 418  5  0  1 NA NA  9.2  4  2  2   2  2  5  1   0  1.86  33   3   0   0
#> 419  5  0  2  1 NA  9.1  2  1  1   3  3  7  1   1  0.96  27   1   0   0
#> 420 11  0  2 NA  4 11.0  6  3  2   1  1  8  0   0  1.64  38   4   2   0
#> 421  1  0 NA NA NA  2.1  0  0  0   2  2  2  0   0  0.00   6   0   0   0
#> 422 10  0  2 NA NA 11.1  5  1  1   3  3  9  0   1  0.79  38   4   1   0
#> 423  5  5  3 NA NA 37.0 16  3  3  11 11 22  0   0  0.73 123  13   3   0
#> 424 10  0 NA NA  8 11.1  3  0  0   4  4 20  0   2  0.00  37   2   1   0
#> 425  3  0 NA NA NA  8.2  6  0  0   0  0  8  0   0  0.00  32   6   0   0
#> 426  9  0 NA NA  8  9.2  3  1  1   5  5 14  0   0  0.93  32   3   0   0
#> 427 10  0 NA NA  1  9.2  5  3  2   1  1 11  0   0  1.86  34   3   2   0
#> 428  9  0 NA NA NA  6.0  3  3  1   1  1  3  0   1  1.50  23   3   0   0
#> 429 10  0  1 NA NA  9.0  1  2  2   6  6  9  0   0  2.00  26   1   0   0
#> 430 10  0 NA NA  6  9.0  5  1  1   0  0  8  0   0  1.00  32   4   1   0
#> 431 10  0  1 NA  3 10.1  3  0  0   2  2 13  0   1  0.00  32   3   0   0
#> 432 11  0 NA NA NA 10.0  3  0  0   2  3 11  0   1  0.00  34   2   1   0
#> 433  1  0 NA NA NA  2.0  1  0  0   0  0  1  0   0  0.00   7   1   0   0
#> 434  9  0  1 NA NA 12.0  2  0  0   4  4 14  0   1  0.00  38   2   0   0
#> 435  8  0  2 NA  3  8.0  3  0  0   1  1 17  0   0  0.00  28   3   0   0
#> 436 11  0  2 NA  1 11.2  4  1  1   2  3 19  0   0  0.77  40   4   0   0
#> 437 11  0 NA NA NA  9.1  2  0  0   2  2 11  0   0  0.00  30   1   1   0
#> 438  9  0 NA  1  1  9.1  3  1  1   1  1  7  0   0  0.96  32   3   0   0
#> 439 12  0 NA NA NA  5.2  2  1  1   0  0  1  0   0  1.59  18   2   0   0
#> 440  9  0  2 NA  4  9.0  2  0  0   1  1 10  0   0  0.00  29   2   0   0
#> 441  2  0 NA NA NA  5.0  1  0  0   0  0  3  0   0  0.00  16   0   1   0
#> 442  4  0 NA NA  4  4.0  0  0  0   1  1  3  0   0  0.00  12   0   0   0
#> 443  5  0 NA NA NA  6.1  1  0  0   0  0  6  0   0  0.00  19   1   0   0
#> 444 10  0 NA NA NA  9.2  1  0  0   0  1  5  0   0  0.00  29   0   1   0
#> 445  7  0  1 NA NA  5.0  0  0  0   0  0  4  0   0  0.00  15   0   0   0
#> 446  1  0 NA NA NA  1.0  0  0  0   0  0  0  0   0  0.00   3   0   0   0
#> 447  1  0 NA NA NA  1.0  0  0  0   0  0  1  0   0  0.00   3   0   0   0
#> 448  2  0 NA NA NA  1.0  0  0  0   0  0  0  0   0  0.00   3   0   0   0
#> 449  1  0 NA NA NA  1.0  0  0  0   0  0  1  0   0  0.00   3   0   0   0
#> 450  1  0 NA NA NA  1.0  0  0  0   0  0  0  0   0  0.00   3   0   0   0
#> 451  1  0 NA NA NA  0.2  0  0  0   0  0  1  0   0  0.00   2   0   0   0
#> 452  1  0 NA NA  1  0.1  0  0  0   0  0  1  0   0  0.00   1   0   0   0
#> 453  1  0 NA NA NA  0.1  0  0  0   0  0  0  0   0  0.00   1   0   0   0
#>     IBB GDP SF SB CS PO  BF Pit  Str  StL  StS GB.FB   LD   PU  WHIP
#> 1     0   0  0  0  0  0  11  49 0.65 0.12 0.08  0.00 0.86 0.00 5.250
#> 2     0   0  0  0  0  1   4   9 0.78 0.22 0.11  0.50 0.25 0.00 2.000
#> 3     0   0  0  0  0  0   2   9 0.11 0.11 0.00    NA   NA   NA    NA
#> 4     0   0  1  1  0  0  14  63 0.63 0.14 0.06  0.36 0.45 0.00 4.000
#> 5     0   1  2  0  0  0  18  60 0.67 0.13 0.03  0.41 0.24 0.00 4.500
#> 6     0   0  0  0  0  0   7  22 0.55 0.14 0.09  0.20 0.20 0.00 4.000
#> 7     0   1  0  0  0  0  16  58 0.60 0.16 0.19  0.44 0.44 0.00 2.667
#> 8     0   0  0  0  0  0   6  18 0.67 0.06 0.17  0.00 0.75 0.00 3.000
#> 9     0   0  0  0  0  0   7  30 0.60 0.10 0.10  0.20 0.40 0.00 4.000
#> 10    0   0  0  2  0  0  13  62 0.60 0.16 0.05  0.13 0.63 0.13 4.800
#> 11    0   0  0  1  0  0  25 101 0.63 0.17 0.13  0.33 0.28 0.06 2.000
#> 12    0   0  0  0  0  0  15  52 0.54 0.13 0.08  0.55 0.18 0.00 3.429
#> 13    0   0  0  0  0  0   4  15 0.87 0.27 0.20  0.25 0.75 0.00 2.000
#> 14    0   1  1  0  0  0   7  31 0.61 0.16 0.03  0.17 0.50 0.00 3.000
#> 15    0   0  0  1  0  0  15  64 0.67 0.17 0.08  0.18 0.36 0.18 2.625
#> 16    1   0  1  0  0  0  17  55 0.55 0.09 0.05  0.43 0.43 0.07 5.000
#> 17    0   0  0  0  0  0  31 123 0.62 0.17 0.10  0.46 0.38 0.00 1.800
#> 18    0   0  0  0  0  0  24 107 0.64 0.14 0.16  0.33 0.56 0.00 1.800
#> 19    0   1  0  3  1  0  24 106 0.58 0.19 0.05  0.42 0.42 0.16 2.786
#> 20    0   0  0  0  0  0   6  24 0.71 0.08 0.13  0.60 0.40 0.00 1.500
#> 21    0   0  0  0  0  0  21  68 0.69 0.19 0.06  0.60 0.20 0.00 2.000
#> 22    0   0  0  1  0  0  92 345 0.61 0.15 0.06  0.30 0.37 0.04 2.208
#> 23    0   0  0  0  0  0   6  19 0.63 0.21 0.11  0.50 0.25 0.00 3.000
#> 24    0   0  1  1  0  0  27 120 0.66 0.17 0.17  0.23 0.54 0.00 2.000
#> 25    0   0  0  0  0  0  29 119 0.58 0.13 0.07  0.61 0.22 0.09 2.294
#> 26    1   0  0  0  1  0  30 124 0.60 0.10 0.13  0.17 0.44 0.06 2.625
#> 27    1   2  0  1  0  0  39 137 0.62 0.17 0.06  0.52 0.29 0.03 2.714
#> 28    2   1  1  0  0  0  50 183 0.60 0.22 0.02  0.48 0.27 0.02 2.276
#> 29    1   0  0  0  0  0  19  78 0.55 0.17 0.04  0.38 0.15 0.00 3.333
#> 30    0   0  0  0  0  0   8  36 0.64 0.11 0.17  0.60 0.00 0.20 3.000
#> 31    0   1  2  1  0  0  98 367 0.62 0.22 0.04  0.41 0.39 0.04 2.017
#> 32    0   0  1  1  0  0  38 143 0.62 0.10 0.15  0.36 0.44 0.04 2.286
#> 33    1   1  0  0  0  0  18  59 0.68 0.17 0.15  0.50 0.42 0.00 3.333
#> 34    0   1  0  2  0  0  41 167 0.57 0.20 0.07  0.45 0.24 0.00 2.250
#> 35    1   5  1  1  0  0 130 454 0.61 0.14 0.06  0.44 0.35 0.10 1.786
#> 36    0   0  0  0  0  0   6  17 0.71 0.18 0.18  0.40 0.40 0.20 3.000
#> 37    1   1  0  0  0  0  13  54 0.56 0.11 0.15  0.25 0.50 0.00 2.571
#> 38    0   0  0  0  0  0  10  32 0.56 0.06 0.09  0.43 0.00 0.14 1.500
#> 39    0   0  0  0  0  0  10  40 0.60 0.18 0.10  0.50 0.17 0.00 1.500
#> 40    0   0  0  0  0  0  31 120 0.70 0.24 0.16  0.55 0.23 0.00 2.167
#> 41    0   2  0  0  0  0  59 218 0.63 0.16 0.12  0.63 0.23 0.10 2.294
#> 42    0   0  0  0  0  0   9  39 0.59 0.13 0.08  0.20 0.80 0.00 2.000
#> 43    1   0  1  2  0  0  42 175 0.61 0.19 0.08  0.27 0.45 0.09 2.857
#> 44    0   1  0  1  0  0  33 125 0.63 0.18 0.12  0.38 0.24 0.05 2.053
#> 45    0   0  2  2  0  0  51 201 0.69 0.15 0.07  0.33 0.45 0.00 1.966
#> 46    0   0  0  0  0  0  24  99 0.60 0.13 0.12  0.29 0.21 0.14 2.308
#> 47    0   1  0  0  0  0  22  78 0.60 0.23 0.05  0.50 0.28 0.00 1.800
#> 48    0   1  0  0  0  0  26  90 0.64 0.14 0.12  0.33 0.19 0.14 2.063
#> 49    0   1  2  3  0  0  92 363 0.63 0.15 0.10  0.37 0.27 0.02 1.929
#> 50    0   0  2  2  0  0  87 336 0.62 0.20 0.07  0.38 0.29 0.09 2.000
#> 51    0   0  0  0  0  0  53 215 0.56 0.17 0.10  0.56 0.19 0.03 2.379
#> 52    0   2  1  1  1  2  83 297 0.59 0.14 0.06  0.42 0.38 0.08 2.020
#> 53    0   0  2  1  0  0  78 310 0.61 0.19 0.06  0.20 0.37 0.11 1.875
#> 54    1   1  1  4  0  0  73 277 0.56 0.18 0.05  0.58 0.30 0.00 2.538
#> 55    0   4  0  1  0  0  42 174 0.68 0.14 0.08  0.58 0.24 0.00 1.600
#> 56    0   0  0  0  0  0  20  90 0.68 0.22 0.12  0.33 0.33 0.20 1.615
#> 57    1   0  0  0  0  0  31 132 0.59 0.14 0.14  0.13 0.50 0.06 2.167
#> 58    1   2  0  0  0  0  43 155 0.61 0.09 0.12  0.45 0.45 0.03 2.111
#> 59    2   3  0  1  0  0  51 171 0.61 0.15 0.08  0.57 0.24 0.02 1.765
#> 60    0   1  0  0  0  0  88 362 0.58 0.15 0.07  0.38 0.27 0.14 1.842
#> 61    0   1  0  2  0  0  38 158 0.63 0.16 0.07  0.48 0.33 0.00 2.087
#> 62    2   2  0  0  0  0  29 112 0.62 0.13 0.13  0.62 0.29 0.05 2.167
#> 63    1   1  0  1  0  0  37 127 0.60 0.12 0.13  0.60 0.28 0.04 2.286
#> 64    0   4  2  0  0  0 107 364 0.64 0.13 0.05  0.42 0.29 0.10 1.732
#> 65    1   0  0  1  0  0  19  64 0.55 0.13 0.09  0.64 0.36 0.00 3.000
#> 66    0   0  1  0  0  0  24  89 0.55 0.13 0.06  0.40 0.40 0.00 2.500
#> 67    0   0  2  0  1  0  71 258 0.65 0.17 0.08  0.49 0.21 0.07 1.408
#> 68    1   4  0  0  1  0 117 438 0.63 0.17 0.06  0.52 0.25 0.06 1.905
#> 69    0   0  0  0  0  0  13  43 0.65 0.19 0.07  0.38 0.23 0.15 1.333
#> 70    0   0  0  0  0  0  14  43 0.67 0.19 0.12  0.67 0.25 0.08 2.250
#> 71    0   1  0  1  0  0  29 117 0.64 0.17 0.09  0.42 0.32 0.05 1.941
#> 72    0   1  0  0  0  0  23  66 0.73 0.27 0.05  0.60 0.30 0.00 1.800
#> 73    0   1  0  3  2  2  97 397 0.63 0.20 0.07  0.36 0.37 0.08 1.545
#> 74    0   2  2  1  1  1 121 463 0.63 0.19 0.10  0.36 0.41 0.08 1.556
#> 75    0   1  1  4  2  0  97 362 0.63 0.18 0.07  0.32 0.30 0.03 1.762
#> 76    0   1  0  1  1  0  94 367 0.69 0.20 0.12  0.38 0.26 0.05 1.217
#> 77    0   0  0  2  0  0  35 133 0.62 0.15 0.13  0.29 0.38 0.08 1.636
#> 78    0   0  0  0  0  0  16  51 0.67 0.16 0.12  0.15 0.23 0.31 1.364
#> 79    0   2  0  1  0  0  40 145 0.63 0.19 0.10  0.31 0.19 0.08 1.345
#> 80    0   0  0  3  0  0  21  94 0.62 0.13 0.16  0.38 0.31 0.08 2.250
#> 81    1   1  0  0  0  0  17  72 0.65 0.14 0.08  0.23 0.31 0.08 2.100
#> 82    0   2  0  1  0  0  49 199 0.66 0.14 0.15  0.53 0.27 0.00 1.588
#> 83    1   0  1  0  1  0  50 188 0.55 0.19 0.06  0.32 0.27 0.05 1.645
#> 84    2   1  0  1  0  0  16  58 0.62 0.17 0.03  0.71 0.21 0.00 2.333
#> 85    0   1  2  1  1  0  91 327 0.63 0.18 0.09  0.26 0.40 0.09 1.429
#> 86    0   4  1  0  0  0  97 376 0.65 0.19 0.08  0.47 0.32 0.03 1.662
#> 87    0   3  0  0  0  0  91 347 0.58 0.18 0.07  0.42 0.22 0.08 1.729
#> 88    1   4  0  2  1  0  87 341 0.57 0.17 0.06  0.57 0.26 0.02 1.678
#> 89    0   2  0  1  4  1  99 357 0.60 0.20 0.05  0.41 0.33 0.04 1.522
#> 90    1   0  1  0  0  0  55 173 0.68 0.17 0.14  0.28 0.23 0.18 1.583
#> 91    1   1  0  0  0  0  21  86 0.59 0.16 0.22  0.45 0.27 0.09 2.077
#> 92    1   3  1  0  2  1 106 400 0.64 0.19 0.09  0.58 0.28 0.00 1.714
#> 93    1   0  0  0  0  0  14  54 0.61 0.13 0.13  0.64 0.27 0.09 2.250
#> 94    0   0  0  0  0  0  40 164 0.62 0.12 0.16  0.36 0.27 0.14 1.320
#> 95    0   1  0  1  2  0 126 464 0.65 0.19 0.09  0.39 0.35 0.10 1.315
#> 96    1   1  1  1  1  0  44 147 0.61 0.18 0.09  0.34 0.34 0.00 1.600
#> 97    0   0  1  0  0  0  21  89 0.47 0.10 0.06  0.25 0.31 0.06 1.615
#> 98    1   1  1  3  1  0  73 266 0.57 0.14 0.07  0.60 0.32 0.00 2.023
#> 99    1   0  0  0  0  0  31 151 0.59 0.11 0.08  0.36 0.23 0.05 2.167
#> 100   0   3  2  0  0  0  39 143 0.65 0.14 0.10  0.48 0.23 0.10 1.393
#> 101   1   3  1  0  0  0 104 396 0.63 0.13 0.09  0.46 0.29 0.03 1.746
#> 102   0   3  0  1  0  0 108 370 0.64 0.21 0.05  0.62 0.27 0.00 1.609
#> 103   1   1  1  1  2  1 109 359 0.62 0.13 0.09  0.48 0.35 0.07 1.520
#> 104   1   4  0  2  1  0  98 358 0.58 0.15 0.07  0.49 0.29 0.10 1.682
#> 105   0   1  0  0  0  0  23 114 0.63 0.18 0.07  0.40 0.27 0.20 1.235
#> 106   1   1  0  0  1  1  30 116 0.59 0.18 0.09  0.58 0.21 0.05 1.429
#> 107   0   5  0  1  0  0 102 401 0.61 0.15 0.09  0.50 0.24 0.04 1.701
#> 108   0   1  2  0  1  0  61 223 0.64 0.16 0.08  0.58 0.19 0.02 1.692
#> 109   0   1  0  1  0  0  97 352 0.68 0.18 0.08  0.30 0.28 0.06 1.343
#> 110   0   0  0  1  0  0  36 142 0.63 0.13 0.11  0.38 0.33 0.08 1.375
#> 111   1   3  2  1  1  0 101 374 0.63 0.19 0.11  0.46 0.34 0.03 1.522
#> 112   1   1  0  1  0  0  50 165 0.68 0.19 0.10  0.30 0.46 0.00 1.727
#> 113   0   2  0  0  0  0 133 479 0.65 0.16 0.08  0.51 0.29 0.05 1.568
#> 114   0   1  0  2  0  0  86 313 0.67 0.23 0.06  0.58 0.26 0.03 1.525
#> 115   0   4  1  0  0  0  51 190 0.63 0.16 0.04  0.53 0.19 0.02 1.459
#> 116   1   1  2  0  0  0  42 162 0.62 0.17 0.09  0.58 0.25 0.03 1.667
#> 117   0   0  2  3  1  0 103 436 0.58 0.19 0.09  0.33 0.29 0.15 1.500
#> 118   1   0  1  0  1  0  95 347 0.63 0.19 0.10  0.42 0.32 0.11 1.831
#> 119   0   1  0  0  0  0   7  36 0.61 0.17 0.03  0.33 0.33 0.33 1.800
#> 120   0   1  1  0  0  0  74 310 0.56 0.17 0.06  0.45 0.28 0.02 1.787
#> 121   0   0  1  8  1  0  96 393 0.63 0.16 0.12  0.43 0.40 0.03 1.569
#> 122   0   3  0  7  1  0 126 452 0.64 0.21 0.07  0.41 0.29 0.06 1.330
#> 123   0   0  2  2  0  0  41 146 0.64 0.11 0.08  0.43 0.35 0.05 1.615
#> 124   0   0  0  0  0  0  37 134 0.68 0.15 0.15  0.44 0.20 0.12 1.440
#> 125   2   0  0  2  2  0  75 297 0.62 0.16 0.09  0.44 0.35 0.04 1.915
#> 126   0   1  0  0  1  0  43 138 0.62 0.19 0.11  0.64 0.25 0.04 1.448
#> 127   0   1  1  0  0  0  22  85 0.61 0.27 0.06  0.31 0.31 0.06 1.400
#> 128   1   2  1  1  0  0  47 190 0.56 0.20 0.04  0.53 0.27 0.07 1.929
#> 129   1   2  0  1  0  0 105 358 0.66 0.17 0.09  0.38 0.35 0.06 1.521
#> 130   0   0  1  1  0  0  27 102 0.56 0.16 0.08  0.60 0.30 0.05 2.063
#> 131   0   2  2  3  2  0 132 440 0.73 0.18 0.07  0.32 0.36 0.11 1.232
#> 132   0   0  2  1  0  0  34 122 0.66 0.17 0.06  0.42 0.08 0.13 1.636
#> 133   1   0  0  1  1  0  51 198 0.60 0.20 0.07  0.47 0.31 0.03 1.818
#> 134   1   0  1  2  2  0  51 165 0.67 0.15 0.09  0.44 0.35 0.00 1.378
#> 135   0   2  0  2  0  0  46 195 0.64 0.18 0.08  0.49 0.26 0.06 1.700
#> 136   0   0  0  1  0  0  16  59 0.66 0.17 0.12  0.62 0.15 0.08 1.800
#> 137   0   1  1  1  1  0 125 454 0.63 0.16 0.09  0.41 0.31 0.08 1.519
#> 138   0   2  0  1  0  0  51 180 0.64 0.15 0.12  0.74 0.14 0.06 1.588
#> 139   0   2  1  4  1  0 141 510 0.68 0.17 0.09  0.39 0.29 0.08 1.375
#> 140   0   1  0  1  0  0  36 136 0.68 0.13 0.15  0.56 0.32 0.00 1.269
#> 141   0   0  0  1  0  0  33 137 0.64 0.23 0.09  0.27 0.36 0.18 1.500
#> 142   0   2  0  4  0  0  37 140 0.61 0.14 0.16  0.43 0.30 0.17 1.500
#> 143   0   0  1  3  1  0  49 191 0.63 0.18 0.10  0.42 0.18 0.18 1.455
#> 144   0   2  0  3  1  0  99 376 0.60 0.16 0.07  0.39 0.22 0.08 1.456
#> 145   1   6  2  0  1  0 107 369 0.69 0.20 0.10  0.54 0.25 0.02 1.364
#> 146   0   3  0  0  1  1  95 370 0.71 0.18 0.12  0.49 0.30 0.06 1.455
#> 147   0   1  0  0  0  0  39 144 0.64 0.17 0.13  0.52 0.24 0.00 1.385
#> 148   0   0  0  1  0  0  12  46 0.61 0.11 0.15  0.33 0.44 0.00 1.500
#> 149   0   2  1  1  0  0  36 158 0.64 0.18 0.09  0.46 0.25 0.08 1.269
#> 150   0   4  1  4  0  0  88 340 0.54 0.19 0.07  0.40 0.30 0.05 1.550
#> 151   0   0  0  1  1  0  35 130 0.58 0.14 0.12  0.26 0.32 0.16 1.435
#> 152   0   0  1  0  0  0   9  28 0.79 0.14 0.18  0.43 0.14 0.00 1.500
#> 153   1   3  0  0  1  0  42 150 0.65 0.17 0.10  0.61 0.23 0.03 1.452
#> 154   1   1  0  2  0  0  51 179 0.63 0.18 0.11  0.78 0.22 0.00 1.781
#> 155   0   1  3  3  0  0 134 495 0.63 0.18 0.08  0.42 0.27 0.07 1.355
#> 156   0   2  1  1  1  1  96 389 0.62 0.17 0.07  0.36 0.20 0.12 1.250
#> 157   1   0  0  7  1  0 128 501 0.58 0.16 0.14  0.53 0.30 0.00 1.627
#> 158   0   1  0  0  0  0  15  63 0.57 0.22 0.06  0.63 0.25 0.13 1.500
#> 159   0   0  3  0  0  0  44 155 0.63 0.15 0.06  0.29 0.14 0.20 1.778
#> 160   0   0  0  2  0  0  21  90 0.63 0.16 0.10  0.19 0.31 0.25 1.500
#> 161   0   4  0  0  1  0 133 496 0.69 0.23 0.07  0.45 0.26 0.08 1.568
#> 162   0   1  0  1  1  0  46 176 0.58 0.16 0.06  0.34 0.38 0.03 1.600
#> 163   1   0  0  1  0  0  18  78 0.55 0.17 0.15  0.33 0.33 0.00 1.636
#> 164   0   0  0  0  0  0  21  76 0.58 0.14 0.11  0.43 0.29 0.14 1.400
#> 165   1   2  1  0  0  0  36 142 0.58 0.15 0.10  0.17 0.38 0.08 1.750
#> 166   0   0  1  0  0  0  31 111 0.63 0.14 0.06  0.33 0.25 0.17 1.429
#> 167   2   3  0  1  0  0 106 385 0.64 0.18 0.07  0.49 0.24 0.04 1.542
#> 168   1   0  1  1  0  0  47 168 0.70 0.15 0.12  0.44 0.28 0.09 1.355
#> 169   1   0  1  1  0  0  30 138 0.62 0.18 0.18  0.27 0.18 0.14 1.737
#> 170   0   0  0  0  0  0  26 105 0.67 0.20 0.11  0.47 0.33 0.07 1.588
#> 171   0   3  0  0  1  0 113 421 0.61 0.17 0.09  0.54 0.25 0.02 1.207
#> 172   0   7  0  1  1  0 123 479 0.63 0.22 0.04  0.53 0.24 0.08 1.483
#> 173   0   2  1  2  1  0 134 513 0.66 0.19 0.12  0.49 0.18 0.08 1.263
#> 174   1   0  1  1  0  0 106 406 0.66 0.18 0.10  0.47 0.25 0.03 1.280
#> 175   0   6  1  4  2  1 126 452 0.58 0.12 0.09  0.52 0.29 0.08 1.517
#> 176   0   0  0  0  0  0  34 132 0.77 0.20 0.11  0.26 0.22 0.13 0.923
#> 177   0   0  0  0  1  1  19  74 0.68 0.23 0.07  0.50 0.33 0.00 1.000
#> 178   0   1  2  1  0  0 116 441 0.65 0.17 0.11  0.51 0.26 0.00 1.560
#> 179   0   5  1  4  0  0 137 511 0.63 0.18 0.09  0.44 0.28 0.04 1.250
#> 180   0   5  0  0  2  0 127 510 0.64 0.18 0.05  0.57 0.20 0.03 1.245
#> 181   0   0  1  3  0  0 126 492 0.66 0.18 0.11  0.35 0.28 0.17 1.281
#> 182   0   0  0  1  0  0  11  39 0.51 0.18 0.05  0.63 0.13 0.00 2.000
#> 183   0   4  1  2  0  0 121 462 0.66 0.17 0.08  0.41 0.34 0.08 1.395
#> 184   1   0  0  0  0  0  44 186 0.61 0.15 0.09  0.33 0.27 0.10 1.778
#> 185   1   0  3  0  0  1 109 384 0.65 0.20 0.05  0.40 0.43 0.04 1.459
#> 186   1   5  0  1  0  0 107 370 0.66 0.18 0.11  0.59 0.22 0.02 1.342
#> 187   0   1  1  2  0  0 121 446 0.61 0.20 0.08  0.52 0.24 0.04 1.425
#> 188   0   1  0  1  0  0  80 316 0.65 0.13 0.17  0.36 0.40 0.02 1.211
#> 189   0   3  0  2  0  0 116 475 0.62 0.20 0.09  0.55 0.29 0.02 1.388
#> 190   0   2  0  1  0  0  92 337 0.61 0.19 0.04  0.45 0.23 0.07 1.364
#> 191   1   2  1  3  3  1  98 362 0.67 0.21 0.08  0.57 0.27 0.03 1.456
#> 192   0   0  1  0  0  0  16  65 0.52 0.11 0.06  0.50 0.20 0.00 1.800
#> 193   0   2  0  0  2  0 107 381 0.63 0.20 0.09  0.56 0.24 0.11 1.231
#> 194   0   0  1  0  0  0  35 114 0.68 0.24 0.06  0.41 0.22 0.15 1.200
#> 195   0   2  0  3  2  1 129 486 0.68 0.18 0.14  0.51 0.30 0.04 1.245
#> 196   1   1  0  0  0  0  96 404 0.62 0.17 0.09  0.35 0.35 0.10 1.343
#> 197   0   4  0  0  0  0  44 173 0.60 0.14 0.10  0.63 0.26 0.00 1.500
#> 198   0   1  0  3  1  1  92 369 0.63 0.17 0.09  0.35 0.29 0.14 1.227
#> 199   0   0  1  0  0  0  48 174 0.67 0.18 0.10  0.47 0.39 0.03 1.500
#> 200   2   5  1  4  0  0  92 345 0.62 0.15 0.10  0.58 0.22 0.03 1.500
#> 201   1   3  2  1  1  0  94 384 0.64 0.18 0.08  0.48 0.28 0.04 1.286
#> 202   1   0  0  1  0  0  47 209 0.60 0.13 0.14  0.32 0.21 0.25 1.452
#> 203   6   2  0  0  0  0  46 160 0.54 0.19 0.08  0.41 0.30 0.11 2.143
#> 204   1   3  1  1  1  0 102 345 0.74 0.23 0.11  0.62 0.20 0.04 1.184
#> 205   0   0  1  2  1  0 100 371 0.60 0.18 0.07  0.55 0.29 0.03 1.324
#> 206   1   0  1  0  0  0  49 181 0.61 0.18 0.07  0.57 0.14 0.05 1.182
#> 207   0   3  0  0  1  0  90 370 0.65 0.16 0.06  0.40 0.26 0.04 1.164
#> 208   0   1  0  0  0  0  13  46 0.65 0.20 0.04  0.33 0.44 0.00 1.667
#> 209   0   0  0  1  0  0  26 102 0.72 0.19 0.10  0.32 0.26 0.11 0.857
#> 210   0   1  0  0  0  0  46 200 0.65 0.16 0.07  0.53 0.31 0.03 1.273
#> 211   0   2  2  2  0  0 120 502 0.62 0.20 0.06  0.53 0.16 0.04 1.235
#> 212   0   1  0  0  0  0  44 159 0.67 0.22 0.10  0.53 0.28 0.06 0.909
#> 213   1   0  0  1  1  0  61 256 0.56 0.14 0.09  0.44 0.27 0.12 1.816
#> 214   0   5  0  0  0  0  88 360 0.62 0.17 0.08  0.44 0.24 0.07 1.403
#> 215   0   3  0  4  1  0 117 429 0.66 0.15 0.07  0.36 0.32 0.13 1.373
#> 216   0   3  0  0  0  0 100 393 0.64 0.18 0.10  0.47 0.34 0.06 1.310
#> 217   0   0  0  1  0  0   8  42 0.62 0.14 0.12  0.67 0.00 0.33 1.800
#> 218   0   0  0  0  0  0  22  93 0.70 0.16 0.12  0.53 0.20 0.00 1.400
#> 219   0   0  0  0  0  0  48 169 0.69 0.25 0.08  0.48 0.26 0.00 0.909
#> 220   0   0  0  2  0  1  87 351 0.65 0.21 0.07  0.58 0.16 0.06 1.210
#> 221   0   0  1  0  0  0   7  20 0.55 0.20 0.00  0.43 0.14 0.00 1.200
#> 222   2   1  0  0  0  0  49 164 0.59 0.15 0.09  0.57 0.29 0.00 1.313
#> 223   0   3  0  0  1  0 132 485 0.68 0.18 0.09  0.38 0.25 0.11 1.213
#> 224   2   3  1  2  0  0  67 247 0.67 0.13 0.14  0.62 0.26 0.03 1.340
#> 225   1   1  2  0  0  0 126 469 0.67 0.17 0.09  0.42 0.25 0.06 1.315
#> 226   1   3  0  3  2  0 129 490 0.63 0.14 0.16  0.44 0.22 0.12 1.129
#> 227   0   2  0  0  0  0  20  70 0.64 0.21 0.07  0.35 0.47 0.00 1.125
#> 228   1   2  0  0  0  0  46 170 0.62 0.25 0.10  0.62 0.28 0.10 1.645
#> 229   0   1  0  1  0  0  44 186 0.62 0.10 0.18  0.38 0.27 0.15 1.182
#> 230   0   0  0  0  0  0  52 189 0.66 0.19 0.07  0.65 0.27 0.00 1.297
#> 231   2   1  1  3  0  0  60 283 0.59 0.13 0.11  0.60 0.20 0.03 1.658
#> 232   0   1  1  0  0  0  51 198 0.66 0.24 0.13  0.39 0.35 0.00 1.105
#> 233   1   2  0  1  0  0  51 202 0.64 0.18 0.10  0.27 0.33 0.15 1.297
#> 234   0   1  0  0  0  0  37 155 0.60 0.14 0.14  0.32 0.32 0.16 1.320
#> 235   1   0  1  2  0  0  97 379 0.63 0.19 0.08  0.57 0.27 0.01 1.141
#> 236   0   7  1  1  0  0 124 425 0.69 0.21 0.08  0.46 0.25 0.06 1.181
#> 237   0   2  0  0  0  0  49 199 0.67 0.18 0.08  0.38 0.33 0.03 1.216
#> 238   1   1  1  0  0  0  88 323 0.66 0.16 0.11  0.36 0.37 0.09 1.377
#> 239   0   1  2  1  2  0 127 475 0.69 0.19 0.11  0.57 0.19 0.05 1.117
#> 240   1   1  1  0  0  0  40 157 0.63 0.20 0.06  0.35 0.32 0.03 1.556
#> 241   2   5  0  5  3  0 141 506 0.66 0.16 0.14  0.52 0.29 0.00 1.265
#> 242   2   1  0  1  2  1 128 519 0.63 0.16 0.11  0.56 0.19 0.11 1.194
#> 243   0   0  0  0  0  0  17  68 0.63 0.18 0.06  0.67 0.33 0.00 1.000
#> 244   0   0  2  1  1  1 130 462 0.65 0.18 0.06  0.43 0.33 0.06 1.133
#> 245   0   2  0  4  3  0 127 487 0.62 0.17 0.08  0.51 0.20 0.06 1.168
#> 246   0   0  0  1  0  0  50 164 0.64 0.18 0.05  0.65 0.23 0.00 1.167
#> 247   0   0  1  0  0  0  20  78 0.59 0.19 0.01  0.50 0.21 0.00 1.500
#> 248   1   1  1  0  0  0  44 164 0.65 0.20 0.05  0.39 0.22 0.03 1.059
#> 249   0   1  0  0  0  0  42 159 0.59 0.17 0.09  0.47 0.19 0.13 1.200
#> 250   0   0  1  0  0  0  17  77 0.65 0.14 0.06  0.64 0.29 0.00 1.636
#> 251   0   2  1  1  0  0  94 388 0.63 0.18 0.07  0.42 0.15 0.11 1.059
#> 252   1   0  1  1  0  0  36 138 0.62 0.12 0.10  0.35 0.26 0.09 1.154
#> 253   0   1  0  0  0  0  25 101 0.58 0.23 0.08  0.53 0.12 0.18 1.167
#> 254   0   0  0  0  0  0  42 173 0.63 0.18 0.14  0.24 0.44 0.04 1.200
#> 255   0   1  0  0  0  0  45 171 0.61 0.18 0.09  0.41 0.24 0.14 1.607
#> 256   0   1  0  1  0  0  38 164 0.66 0.15 0.18  0.53 0.24 0.12 1.200
#> 257   0   2  0  1  0  0  76 279 0.60 0.21 0.08  0.52 0.21 0.14 1.389
#> 258   0   1  0  0  0  0  32 112 0.67 0.20 0.08  0.57 0.22 0.13 1.125
#> 259   0   2  0  1  1  0  43 162 0.68 0.13 0.14  0.53 0.30 0.03 1.182
#> 260   0   4  0  4  0  0  98 376 0.62 0.18 0.08  0.58 0.15 0.04 1.125
#> 261   0   0  2  0  0  0  29 106 0.73 0.20 0.08  0.39 0.30 0.04 1.143
#> 262   0   1  1  0  0  0 107 352 0.70 0.18 0.10  0.49 0.30 0.10 1.208
#> 263   1   0  0  0  0  0  38 154 0.56 0.12 0.12  0.50 0.25 0.00 1.560
#> 264   0   1  0  1  0  0  23  86 0.62 0.16 0.12  0.38 0.38 0.13 1.235
#> 265   0   0  0  1  1  1  40 165 0.68 0.19 0.13  0.35 0.22 0.04 0.844
#> 266   1   0  1  1  1  0 102 413 0.63 0.22 0.07  0.26 0.29 0.21 1.225
#> 267   0   2  0  0  0  0  30 142 0.56 0.17 0.10  0.53 0.29 0.18 1.364
#> 268   0   3  2  0  1  0 129 433 0.71 0.22 0.07  0.41 0.27 0.06 0.980
#> 269   1   2  2  0  0  0 132 509 0.64 0.18 0.10  0.40 0.27 0.04 1.258
#> 270   0   1  0  1  0  0  20  60 0.73 0.20 0.07  0.47 0.24 0.00 0.938
#> 271   0   4  1  1  1  0 101 384 0.66 0.17 0.11  0.47 0.31 0.08 1.037
#> 272   2   1  0  1  1  0  45 177 0.59 0.15 0.13  0.83 0.07 0.00 1.759
#> 273   0   2  1  1  0  0  97 397 0.62 0.20 0.10  0.44 0.22 0.07 1.099
#> 274   1   0  0  4  0  0  42 157 0.62 0.12 0.12  0.17 0.24 0.10 1.393
#> 275   0   3  0  2  0  0  38 162 0.63 0.23 0.10  0.48 0.28 0.00 1.071
#> 276   0   1  0  0  2  1 108 377 0.67 0.22 0.07  0.44 0.36 0.06 1.061
#> 277   0   1  0  0  1  0 100 387 0.66 0.21 0.11  0.56 0.25 0.04 1.176
#> 278   0   1  1  0  0  0  23 112 0.69 0.14 0.27  0.17 0.50 0.17 0.789
#> 279   1   4  0  7  0  0 101 377 0.64 0.19 0.09  0.57 0.21 0.07 1.240
#> 280   0   3  2  0  0  0 103 382 0.66 0.22 0.07  0.43 0.21 0.04 1.026
#> 281   1   0  0  2  0  0  29 110 0.65 0.16 0.06  0.55 0.35 0.00 1.500
#> 282   0   2  0  1  0  0  92 369 0.63 0.21 0.05  0.36 0.21 0.13 0.917
#> 283   1   2  0  2  0  0  39 152 0.59 0.20 0.08  0.10 0.45 0.14 1.200
#> 284   0   1  0  1  0  0  50 175 0.59 0.17 0.10  0.56 0.31 0.06 1.200
#> 285   1   0  1  1  0  0  37 134 0.67 0.22 0.18  0.41 0.07 0.21 1.000
#> 286   0   0  0  0  0  0  15  52 0.71 0.13 0.04  0.18 0.27 0.18 1.500
#> 287   2   2  0  3  0  0 119 468 0.65 0.17 0.08  0.49 0.33 0.04 1.138
#> 288   0   1  1  1  0  0  19  77 0.61 0.14 0.19  0.71 0.21 0.00 1.385
#> 289   0   0  0  0  0  0  23  87 0.67 0.23 0.05  0.65 0.18 0.06 0.882
#> 290   1   0  1  0  1  0  55 201 0.63 0.13 0.10  0.29 0.24 0.18 1.231
#> 291   1   6  0  4  2  0  85 324 0.62 0.18 0.11  0.58 0.18 0.07 1.119
#> 292   1   3  0  1  0  0  36 127 0.62 0.17 0.13  0.54 0.17 0.04 1.179
#> 293   0   1  0  0  0  0  10  38 0.68 0.29 0.00  0.88 0.00 0.00 1.286
#> 294   1   0  0  0  0  0  94 363 0.65 0.19 0.09  0.31 0.34 0.09 1.056
#> 295   0   1  0  0  0  0  31  98 0.66 0.14 0.07  0.52 0.22 0.13 1.043
#> 296   0   0  0  0  0  0  32 141 0.73 0.20 0.18  0.31 0.38 0.13 1.000
#> 297   0   0  0  2  0  0  40 158 0.68 0.13 0.11  0.43 0.33 0.03 1.000
#> 298   0   0  0  0  0  0  18  56 0.68 0.20 0.13  0.40 0.13 0.07 0.857
#> 299   0   0  0  1  0  0  41 160 0.68 0.17 0.07  0.24 0.22 0.16 0.968
#> 300   1   0  0  1  0  0  25 102 0.57 0.15 0.08  0.22 0.33 0.17 1.412
#> 301   1   0  0  0  1  1 102 392 0.65 0.17 0.09  0.51 0.28 0.06 1.013
#> 302   0   4  0  1  1  0  73 301 0.63 0.20 0.09  0.63 0.10 0.10 1.222
#> 303   1   2  0  0  0  0  41 168 0.59 0.13 0.11  0.48 0.12 0.12 1.100
#> 304   0   0  0  0  1  1  36 154 0.64 0.17 0.11  0.48 0.38 0.05 1.320
#> 305   0   1  0  1  1  0 107 374 0.68 0.20 0.13  0.43 0.22 0.03 0.938
#> 306   1   0  0  1  0  0   8  36 0.44 0.17 0.03  0.50 0.00 0.50 3.000
#> 307   0   2  0  1  0  0  98 340 0.65 0.15 0.08  0.51 0.29 0.03 1.040
#> 308   2   1  0  0  0  0  19  69 0.59 0.19 0.09  0.50 0.08 0.17 1.200
#> 309   1   1  0  2  0  0  39 170 0.62 0.16 0.05  0.52 0.18 0.09 1.065
#> 310   0   0  1  0  0  0  31 124 0.65 0.23 0.09  0.50 0.25 0.04 0.875
#> 311   0   3  0  0  0  0  95 355 0.60 0.17 0.08  0.57 0.19 0.05 0.908
#> 312   0   4  1  0  1  0  87 330 0.65 0.16 0.08  0.52 0.34 0.03 0.955
#> 313   0   0  1  0  0  0  39 170 0.61 0.15 0.13  0.55 0.15 0.05 1.385
#> 314   0   2  0  0  0  0  72 286 0.64 0.20 0.09  0.52 0.30 0.07 1.036
#> 315   1   0  0  2  0  0  42 181 0.60 0.15 0.10  0.43 0.26 0.09 1.500
#> 316   0   1  0  1  0  0 107 365 0.67 0.15 0.09  0.58 0.19 0.06 1.038
#> 317   1   1  1  2  0  1 109 401 0.61 0.18 0.06  0.41 0.24 0.13 1.154
#> 318   0   3  1  0  0  1 105 384 0.66 0.16 0.09  0.42 0.33 0.08 1.115
#> 319   0   1  0  0  0  0  36 138 0.64 0.19 0.15  0.55 0.27 0.05 1.222
#> 320   0   0  0  0  1  0  40 160 0.66 0.14 0.11  0.33 0.19 0.19 0.844
#> 321   0   1  0  0  1  0  23  81 0.60 0.26 0.06  0.56 0.19 0.06 1.235
#> 322   0   2  0  0  0  0  39 154 0.64 0.21 0.10  0.45 0.21 0.10 0.938
#> 323   0   1  0  0  1  0  30 115 0.67 0.18 0.07  0.50 0.33 0.06 1.174
#> 324   0   0  1  0  1  1  40 156 0.69 0.22 0.14  0.22 0.30 0.00 1.034
#> 325   0   0  0  0  0  0  84 308 0.62 0.19 0.06  0.52 0.26 0.06 0.891
#> 326   0   0  0  0  0  0  11  45 0.56 0.20 0.07  0.25 0.25 0.25 1.714
#> 327   1   0  0  1  0  0  26  95 0.68 0.15 0.14  0.50 0.11 0.11 1.105
#> 328   0   1  0  0  0  0  26 103 0.65 0.19 0.13  0.33 0.44 0.06 0.857
#> 329   0   2  0  3  1  0  99 406 0.61 0.16 0.11  0.37 0.27 0.17 1.120
#> 330   0   3  1  0  0  0  34 130 0.62 0.19 0.12  0.50 0.27 0.00 1.111
#> 331   2   0  0  0  0  0  42 145 0.68 0.12 0.08  0.42 0.10 0.29 1.100
#> 332   0   0  1  2  0  0  34 132 0.70 0.20 0.12  0.68 0.21 0.00 0.840
#> 333   0   0  1  1  0  0  60 243 0.65 0.16 0.13  0.29 0.29 0.17 0.913
#> 334   1   0  0  2  0  0  74 277 0.63 0.17 0.12  0.38 0.33 0.13 1.132
#> 335   0   0  0  0  0  0  23  97 0.59 0.19 0.10  0.44 0.19 0.13 1.000
#> 336   0   0  0  0  0  0  10  40 0.63 0.08 0.10  0.71 0.00 0.14 1.286
#> 337   0   0  0  0  0  0  36 148 0.64 0.16 0.11  0.38 0.13 0.08 0.857
#> 338   3   0  0  0  0  0  46 192 0.63 0.15 0.10  0.46 0.25 0.04 1.400
#> 339   0   2  0  1  1  0 125 482 0.66 0.18 0.09  0.32 0.31 0.11 0.959
#> 340   0   0  1  0  0  0  78 299 0.61 0.21 0.05  0.27 0.31 0.10 1.179
#> 341   0   3  0  1  1  0 128 513 0.63 0.17 0.10  0.49 0.29 0.04 0.918
#> 342   3   2  2  0  1  0 135 503 0.65 0.21 0.07  0.45 0.28 0.06 0.897
#> 343   0   1  0  0  0  0  27  93 0.59 0.16 0.12  0.19 0.38 0.19 0.789
#> 344   0   1  0  1  0  0  46 215 0.62 0.18 0.11  0.52 0.16 0.12 0.943
#> 345   0   0  0  0  0  0  20  89 0.67 0.22 0.06  0.69 0.00 0.00 1.000
#> 346   0   0  2  1  0  0  90 358 0.64 0.21 0.10  0.38 0.22 0.12 0.940
#> 347   0   0  0  2  0  0  34 139 0.62 0.15 0.12  0.30 0.17 0.17 0.808
#> 348   0   0  0  2  1  0  31 106 0.62 0.18 0.09  0.68 0.23 0.00 1.125
#> 349   0   1  0  0  1  0  47 159 0.59 0.16 0.08  0.71 0.18 0.00 1.114
#> 350   0   0  0  0  0  0  37 151 0.64 0.15 0.13  0.75 0.21 0.00 0.964
#> 351   0   0  0  2  0  0  43 157 0.68 0.15 0.18  0.44 0.22 0.15 0.938
#> 352   0   2  0  0  0  0  40 138 0.64 0.12 0.13  0.53 0.20 0.03 1.000
#> 353   0   0  0  0  0  0  37 177 0.64 0.19 0.14  0.37 0.11 0.11 1.071
#> 354   0   1  1  0  0  0  16  53 0.74 0.23 0.06  0.67 0.17 0.08 0.923
#> 355   0   0  0  0  0  0  36 161 0.63 0.14 0.11  0.68 0.20 0.04 0.923
#> 356   1   2  1  2  1  0  54 197 0.62 0.20 0.10  0.65 0.24 0.00 1.171
#> 357   0   1  0  0  0  0   9  46 0.50 0.22 0.09  1.00 0.00 0.00 1.286
#> 358   1   0  1  0  1  1  50 207 0.62 0.17 0.13  0.19 0.50 0.03 1.000
#> 359   0   3  0  2  0  0 121 485 0.66 0.18 0.12  0.62 0.26 0.01 0.978
#> 360   0   3  1  1  0  0  60 223 0.68 0.16 0.09  0.44 0.27 0.02 0.894
#> 361   0   1  0  0  1  0  46 170 0.75 0.21 0.15  0.40 0.23 0.07 0.692
#> 362   0   0  0  0  0  0  24  97 0.66 0.25 0.11  0.50 0.17 0.06 1.000
#> 363   0   0  0  1  0  0  49 206 0.69 0.16 0.15  0.44 0.28 0.11 0.769
#> 364   0   0  0  0  0  0  29 118 0.64 0.27 0.08  0.33 0.27 0.13 0.955
#> 365   1   1  0  0  0  0  25  91 0.64 0.16 0.13  0.60 0.25 0.05 1.105
#> 366   0   0  0  3  0  0 105 419 0.66 0.19 0.08  0.53 0.24 0.07 0.938
#> 367   0   0  0  0  1  0  41 163 0.61 0.16 0.15  0.46 0.17 0.25 0.844
#> 368   0   3  0  1  0  0 131 505 0.65 0.17 0.12  0.67 0.11 0.08 0.808
#> 369   1   1  0  0  0  0  40 148 0.70 0.24 0.10  0.41 0.33 0.11 1.100
#> 370   0   0  0  0  1  1  21  98 0.59 0.20 0.08  0.62 0.15 0.08 1.125
#> 371   0   1  0  0  0  0   7  19 0.58 0.26 0.00  0.50 0.33 0.17 1.000
#> 372   0   0  0  3  1  2  96 382 0.62 0.18 0.15  0.46 0.31 0.07 0.945
#> 373   0   1  0  2  0  0  39 157 0.68 0.18 0.15  0.39 0.39 0.04 0.871
#> 374   0   0  0  1  0  0   8  27 0.70 0.11 0.15  0.86 0.00 0.00 1.000
#> 375   0   3  0  1  1  0  77 330 0.59 0.21 0.07  0.60 0.18 0.04 1.000
#> 376   0   1  1  1  0  0  49 203 0.63 0.18 0.18  0.50 0.31 0.04 0.868
#> 377   1   1  2  2  0  0  53 201 0.66 0.16 0.11  0.35 0.20 0.05 0.857
#> 378   0   4  0  1  1  0 136 496 0.66 0.19 0.08  0.45 0.27 0.11 0.881
#> 379   1   0  0  0  0  0  30 114 0.60 0.16 0.15  0.44 0.25 0.13 1.091
#> 380   1   2  1  0  1  0  40 159 0.68 0.14 0.17  0.50 0.13 0.17 0.818
#> 381   1   1  1  3  0  0  42 156 0.68 0.13 0.21  0.42 0.31 0.12 1.065
#> 382   0   0  0  0  0  0  10  29 0.69 0.17 0.07  0.50 0.50 0.00 0.667
#> 383   0   1  1  5  0  0 130 490 0.64 0.18 0.10  0.46 0.22 0.06 0.861
#> 384   0   0  0  1  0  0  24  87 0.66 0.24 0.09  0.50 0.28 0.06 1.059
#> 385   0   1  0  0  0  0  31 125 0.66 0.22 0.10  0.45 0.14 0.05 0.600
#> 386   2   1  0  1  0  0  24  92 0.62 0.16 0.16  0.86 0.14 0.00 1.500
#> 387   1   2  2  0  0  0 138 500 0.69 0.16 0.12  0.41 0.28 0.08 0.730
#> 388   1   0  0  2  1  0  34 130 0.64 0.11 0.18  0.53 0.37 0.00 1.200
#> 389   0   1  1  0  2  0 109 396 0.68 0.21 0.13  0.47 0.23 0.04 0.847
#> 390   0   2  0  0  0  0 106 395 0.66 0.18 0.12  0.48 0.22 0.08 0.878
#> 391   0   4  0  3  0  0  84 346 0.61 0.25 0.06  0.67 0.17 0.02 0.794
#> 392   1   0  1  2  0  0  17  65 0.62 0.12 0.12  0.55 0.27 0.09 1.250
#> 393   0   1  0  0  0  0  35 135 0.70 0.20 0.15  0.48 0.38 0.05 0.857
#> 394   0   1  0  1  1  0 121 477 0.67 0.20 0.14  0.60 0.17 0.06 0.742
#> 395   0   0  0  1  0  0  29 123 0.60 0.10 0.12  0.43 0.33 0.05 1.000
#> 396   1   1  0  0  0  0  34 121 0.60 0.13 0.12  0.40 0.20 0.08 0.889
#> 397   0   0  0  0  0  0  32 126 0.71 0.16 0.11  0.40 0.20 0.05 0.600
#> 398   0   1  0  2  0  0 113 381 0.71 0.18 0.12  0.38 0.22 0.13 0.837
#> 399   1   2  0  3  0  0  99 342 0.67 0.17 0.12  0.41 0.25 0.12 0.769
#> 400   0   0  0  0  0  0  20  86 0.65 0.16 0.13  0.75 0.25 0.00 1.000
#> 401   0   0  0  0  0  0  34 146 0.64 0.22 0.06  0.45 0.23 0.00 0.923
#> 402   0   0  0  0  0  0  42 157 0.61 0.20 0.10  0.47 0.27 0.03 0.909
#> 403   0   0  0  1  0  0  45 200 0.65 0.22 0.08  0.55 0.31 0.00 0.818
#> 404   0   0  1  0  0  0  33 129 0.63 0.22 0.07  0.35 0.35 0.08 0.667
#> 405   0   0  0  1  0  0  28 121 0.55 0.13 0.13  0.56 0.13 0.13 1.000
#> 406   1   0  0  1  0  0  29 108 0.61 0.22 0.14  0.31 0.19 0.13 0.955
#> 407   0   0  0  2  1  0  49 202 0.60 0.23 0.13  0.57 0.17 0.13 0.973
#> 408   0   1  0  0  0  0  38 144 0.65 0.21 0.16  0.62 0.19 0.00 0.656
#> 409   0   0  0  0  0  0  13  46 0.65 0.30 0.00  1.00 0.00 0.00 0.900
#> 410   0   0  0  0  0  0  12  48 0.65 0.21 0.10  0.50 0.17 0.17 1.000
#> 411   0   2  0  0  0  0  19  72 0.67 0.24 0.10  0.73 0.18 0.00 0.706
#> 412   0   0  0  1  0  0  35 157 0.61 0.21 0.10  0.23 0.32 0.09 0.889
#> 413   0   0  0  3  0  0  41 162 0.70 0.17 0.25  0.44 0.06 0.17 0.750
#> 414   0   1  0  0  0  0  21  86 0.64 0.17 0.09  0.14 0.36 0.29 0.882
#> 415   0   0  0  0  0  0  31 132 0.64 0.17 0.15  0.33 0.22 0.00 0.462
#> 416   3   1  0  0  0  0  40 155 0.61 0.19 0.10  0.88 0.12 0.00 1.100
#> 417   0   0  0  0  0  0  40 161 0.65 0.26 0.05  0.43 0.20 0.07 0.656
#> 418   0   0  0  0  0  0  35 149 0.62 0.14 0.12  0.31 0.21 0.14 0.621
#> 419   0   0  1  0  1  0  33 119 0.61 0.18 0.10  0.29 0.19 0.14 0.536
#> 420   0   1  0  0  0  0  39 137 0.73 0.19 0.13  0.67 0.20 0.03 0.636
#> 421   0   0  0  0  1  1   8  31 0.58 0.23 0.13  0.50 0.25 0.00 0.857
#> 422   0   0  0  1  0  0  44 183 0.67 0.14 0.09  0.52 0.14 0.10 0.706
#> 423   0   3  0  0  0  0 135 523 0.62 0.20 0.07  0.63 0.23 0.02 0.730
#> 424   0   0  0  0  0  0  43 176 0.63 0.25 0.18  0.53 0.24 0.06 0.618
#> 425   0   0  0  0  0  0  32 111 0.71 0.24 0.07  0.33 0.25 0.08 0.692
#> 426   0   0  0  0  0  0  37 160 0.61 0.17 0.14  0.44 0.17 0.00 0.828
#> 427   0   0  1  1  0  0  36 155 0.71 0.14 0.16  0.13 0.33 0.29 0.621
#> 428   0   0  0  1  0  0  25  80 0.64 0.19 0.05  0.60 0.20 0.10 0.667
#> 429   0   1  1  0  0  0  33 140 0.54 0.13 0.12  0.44 0.06 0.17 0.778
#> 430   0   0  0  0  0  0  32 106 0.72 0.14 0.13  0.33 0.33 0.04 0.556
#> 431   0   2  0  2  1  0  35 142 0.63 0.22 0.13  0.63 0.05 0.11 0.484
#> 432   1   0  0  1  0  0  38 150 0.68 0.23 0.13  0.61 0.13 0.00 0.600
#> 433   0   0  0  0  0  0   7  22 0.73 0.32 0.00  0.17 0.50 0.17 0.500
#> 434   0   0  0  1  0  0  43 164 0.65 0.23 0.10  0.50 0.29 0.04 0.500
#> 435   0   0  0  2  0  0  29 120 0.73 0.25 0.22  0.55 0.45 0.00 0.500
#> 436   1   0  0  0  0  0  43 180 0.67 0.19 0.14  0.38 0.10 0.19 0.600
#> 437   0   0  0  0  0  0  32 124 0.63 0.18 0.15  0.63 0.16 0.05 0.429
#> 438   0   0  1  1  0  0  34 142 0.68 0.20 0.11  0.54 0.12 0.08 0.429
#> 439   0   1  0  0  0  0  18  67 0.63 0.10 0.04  0.76 0.06 0.00 0.353
#> 440   0   0  0  0  0  0  30 110 0.66 0.18 0.12  0.47 0.26 0.05 0.333
#> 441   0   0  0  0  0  0  16  52 0.71 0.19 0.13  0.62 0.38 0.00 0.200
#> 442   0   0  0  0  0  0  13  37 0.73 0.22 0.11  0.67 0.00 0.11 0.250
#> 443   0   1  0  0  0  0  19  84 0.65 0.23 0.08  0.54 0.23 0.00 0.158
#> 444   1   1  0  0  0  0  30 120 0.64 0.12 0.10  0.79 0.13 0.00 0.207
#> 445   0   1  0  2  0  0  15  55 0.65 0.25 0.11  0.58 0.33 0.00 0.000
#> 446   0   0  0  0  0  0   3   8 0.75 0.25 0.00  0.67 0.00 0.00 0.000
#> 447   0   0  0  0  0  0   3  10 0.70 0.10 0.20  0.50 0.00 0.00 0.000
#> 448   0   0  0  0  0  0   3   9 0.78 0.22 0.11  0.00 1.00 0.00 0.000
#> 449   0   0  0  0  0  0   3   8 0.75 0.00 0.38  0.00 0.50 0.50 0.000
#> 450   0   0  0  0  0  0   3   9 0.67 0.11 0.11  1.00 0.00 0.00 0.000
#> 451   0   0  0  0  0  0   2   7 0.71 0.14 0.29  0.00 1.00 0.00 0.000
#> 452   0   0  0  0  0  0   1   4 0.75 0.00 0.50    NA   NA   NA 0.000
#> 453   0   0  0  0  0  0   1   1 1.00 0.00 0.00  0.00 0.00 1.00 0.000
#>     BAbip  SO9  SO.W SO_perc uBB_perc SO_uBB    FIP wOBA_against
#> 1   0.833 20.3  3.00   0.273    0.091      0  12.22        0.784
#> 2   0.333  0.0    NA   0.000    0.000      0  16.13        0.736
#> 3      NA   NA  0.00   0.000    1.000     -1    Inf        0.687
#> 4   0.500  4.5  0.50   0.071    0.143      0  11.63        0.667
#> 5   0.625  0.0  0.00   0.000    0.111      0   5.86        0.647
#> 6   0.500  9.0  1.00   0.143    0.143      0  17.13        0.645
#> 7   0.571 12.0  2.00   0.250    0.125      0  11.13        0.632
#> 8   0.333  9.0  1.00   0.167    0.167      0  17.13        0.606
#> 9   0.600  9.0  1.00   0.143    0.143      0   4.13        0.583
#> 10  0.750 10.8  1.00   0.154    0.154      0   4.80        0.570
#> 11  0.357 10.8  6.00   0.240    0.040      0  11.73        0.564
#> 12  0.600 11.6  3.00   0.200    0.067      0   7.90        0.561
#> 13  0.667  9.0    NA   0.250    0.000      0   1.13        0.534
#> 14  0.500  0.0  0.00   0.000    0.143      0   5.86        0.529
#> 15  0.455  6.8  2.00   0.133    0.067      0   8.59        0.527
#> 16  0.643  4.5  1.00   0.059    0.000      0   2.13        0.511
#> 17  0.350  5.4  2.00   0.129    0.065      0   9.59        0.503
#> 18  0.333  9.0  5.00   0.208    0.042      0   9.53        0.486
#> 19  0.500  3.9  0.50   0.083    0.167      0   5.04        0.476
#> 20  0.400  6.8    NA   0.167    0.000      0   1.32        0.475
#> 21  0.357  4.5  2.00   0.095    0.048      0  10.13        0.473
#> 22  0.364  5.1  1.11   0.109    0.098      0   8.25        0.471
#> 23  0.500  9.0  1.00   0.167    0.167      0   4.13        0.471
#> 24  0.500 19.8  5.50   0.407    0.074      0   8.33        0.470
#> 25  0.400  6.4  1.33   0.138    0.103      0   8.33        0.469
#> 26  0.444 10.1  1.20   0.200    0.133      0   5.68        0.468
#> 27  0.483  5.1  1.33   0.103    0.051      0   6.56        0.467
#> 28  0.333  1.9  0.33   0.040    0.080      0   8.24        0.466
#> 29  0.462  6.0  0.50   0.105    0.158      0   4.80        0.464
#> 30  0.600 13.5  2.00   0.250    0.125      0   2.22        0.463
#> 31  0.328  7.9  1.55   0.173    0.112      0   8.68        0.463
#> 32  0.500 12.9  3.33   0.263    0.079      0   3.42        0.455
#> 33  0.667 12.0  2.00   0.222    0.056      0   1.47        0.451
#> 34  0.308  5.6  0.71   0.122    0.171      0   9.38        0.449
#> 35  0.341  5.8  1.64   0.138    0.077      0   6.85        0.441
#> 36  0.750 18.0    NA   0.333    0.000      0  -0.87        0.440
#> 37  0.500  7.7  1.00   0.154    0.077      0   4.09        0.437
#> 38  0.167  4.5  1.00   0.100    0.100      0  11.63        0.435
#> 39  0.200  9.0  2.00   0.200    0.100      0  10.63        0.435
#> 40  0.450 10.5  3.50   0.226    0.065      0   6.13        0.433
#> 41  0.439  7.1  1.29   0.153    0.119      0   4.85        0.429
#> 42  0.333  4.5  0.50   0.111    0.222      0   5.13        0.428
#> 43  0.591 14.1  1.57   0.262    0.143      0   2.99        0.424
#> 44  0.400  9.9  2.33   0.212    0.091      0   7.07        0.423
#> 45  0.361  7.4  2.67   0.157    0.059      0   6.94        0.423
#> 46  0.308  8.3  0.80   0.167    0.208      0   8.74        0.422
#> 47  0.294  1.8  0.33   0.045    0.136      0   7.13        0.422
#> 48  0.400  5.1  1.50   0.115    0.077      0   5.68        0.418
#> 49  0.439 10.6  3.14   0.239    0.076      0   4.89        0.418
#> 50  0.355  6.4  1.33   0.138    0.103      0   5.78        0.416
#> 51  0.433 11.2  1.33   0.226    0.170      0   5.20        0.414
#> 52  0.344  3.9  0.78   0.084    0.108      0   6.92        0.412
#> 53  0.396  9.0  2.29   0.205    0.090      0   4.07        0.409
#> 54  0.392  4.8  0.58   0.096    0.151      0   6.06        0.408
#> 55  0.333  5.4  2.00   0.143    0.071      0   6.73        0.406
#> 56  0.429 10.4    NA   0.250    0.000      0   3.87        0.405
#> 57  0.500 18.0  2.40   0.387    0.129      0   5.47        0.404
#> 58  0.419  6.0  1.20   0.140    0.093      0   4.58        0.404
#> 59  0.317  2.4  0.75   0.059    0.039      0   6.65        0.402
#> 60  0.316  6.2  1.00   0.148    0.148      0   6.56        0.401
#> 61  0.423  8.2  1.75   0.184    0.105      0   4.66        0.400
#> 62  0.524  9.0  3.00   0.207    0.000      0   1.13        0.399
#> 63  0.435  9.0  1.40   0.189    0.108      0   4.71        0.399
#> 64  0.364  3.8  2.00   0.093    0.047      0   5.16        0.397
#> 65  0.538  8.1  1.00   0.158    0.105      0   3.13        0.397
#> 66  0.400  9.0  1.00   0.167    0.167      0   4.88        0.396
#> 67  0.294  6.1  3.67   0.155    0.042      0   6.36        0.392
#> 68  0.353  4.4  0.80   0.103    0.120      0   5.20        0.391
#> 69  0.250  0.0    NA   0.000    0.000      0   7.47        0.391
#> 70  0.455  6.8  2.00   0.143    0.071      0   2.68        0.390
#> 71  0.333  7.9  1.25   0.172    0.138      0   6.60        0.386
#> 72  0.400  3.6  2.00   0.087    0.043      0   2.93        0.385
#> 73  0.366  6.5  3.20   0.165    0.052      0   4.41        0.385
#> 74  0.316  7.3  1.83   0.182    0.099      0   6.06        0.385
#> 75  0.382  7.3  1.89   0.175    0.093      0   4.04        0.381
#> 76  0.288 11.3  7.25   0.309    0.043      0   6.22        0.380
#> 77  0.364 11.0  4.50   0.257    0.057      0   5.11        0.379
#> 78  0.308  2.5  1.00   0.062    0.062      0   4.38        0.378
#> 79  0.217  7.4  1.60   0.200    0.125      0   7.59        0.378
#> 80  0.538 13.5  3.00   0.286    0.095      0   1.63        0.377
#> 81  0.462  8.1  3.00   0.176    0.000      0   1.20        0.377
#> 82  0.429 11.1  2.80   0.286    0.102      0   3.13        0.374
#> 83  0.212  4.4  0.71   0.100    0.120      0   8.38        0.373
#> 84  0.308  0.0  0.00   0.000    0.000      0   7.47        0.373
#> 85  0.328  7.3  3.40   0.187    0.055      0   4.85        0.373
#> 86  0.420  8.3  4.00   0.206    0.052      0   3.32        0.372
#> 87  0.324  4.1  1.13   0.099    0.088      0   5.48        0.372
#> 88  0.283  7.3  1.14   0.184    0.149      0   6.21        0.371
#> 89  0.288  3.9  1.00   0.101    0.101      0   5.96        0.370
#> 90  0.316  7.5  2.00   0.182    0.073      0   4.63        0.370
#> 91  0.545 14.5  2.33   0.333    0.095      0   1.18        0.369
#> 92  0.394  8.5  2.00   0.208    0.094      0   3.22        0.368
#> 93  0.500 10.1  3.00   0.214    0.000      0   0.41        0.368
#> 94  0.300 11.9  5.50   0.275    0.050      0   6.71        0.367
#> 95  0.286  5.8  3.80   0.151    0.040      0   6.01        0.367
#> 96  0.333  7.2  2.00   0.182    0.068      0   5.03        0.366
#> 97  0.133  0.0  0.00   0.000    0.190      0   9.96        0.365
#> 98  0.357  3.1  0.63   0.068    0.096      0   5.05        0.365
#> 99  0.455  9.0  2.00   0.194    0.065      0   2.13        0.364
#> 100 0.300  4.8  2.50   0.128    0.051      0   5.55        0.364
#> 101 0.390  6.4  2.00   0.154    0.067      0   3.22        0.363
#> 102 0.313  5.5  1.56   0.130    0.083      0   4.92        0.363
#> 103 0.330  4.3  2.00   0.110    0.046      0   4.33        0.363
#> 104 0.324  5.3  1.00   0.133    0.122      0   4.91        0.362
#> 105 0.154  7.9  1.67   0.217    0.130      0   7.94        0.362
#> 106 0.333  9.0  3.50   0.233    0.033      0   5.71        0.361
#> 107 0.300  2.4  0.50   0.059    0.118      0   5.53        0.361
#> 108 0.362  4.8  1.75   0.115    0.066      0   4.21        0.360
#> 109 0.303  6.4  2.67   0.165    0.062      0   4.99        0.359
#> 110 0.286 10.1  4.50   0.250    0.056      0   6.51        0.357
#> 111 0.385  9.4  3.43   0.238    0.059      0   3.66        0.357
#> 112 0.429  9.0  3.67   0.220    0.040      0   2.86        0.356
#> 113 0.295  5.8  1.46   0.143    0.098      0   5.51        0.353
#> 114 0.348  5.0  2.75   0.128    0.047      0   3.97        0.352
#> 115 0.304  0.0  0.00   0.000    0.059      0   4.95        0.352
#> 116 0.294  3.0  0.75   0.071    0.071      0   4.91        0.352
#> 117 0.299  6.9  1.50   0.175    0.117      0   5.08        0.351
#> 118 0.387  8.7  1.73   0.200    0.105      0   3.71        0.351
#> 119 0.333  0.0  0.00   0.000    0.143      0   5.63        0.350
#> 120 0.308  5.7  0.91   0.135    0.149      0   4.84        0.350
#> 121 0.424 10.0  4.80   0.250    0.052      0   2.19        0.349
#> 122 0.284  4.0  3.25   0.103    0.032      0   5.44        0.349
#> 123 0.306  2.1  1.00   0.049    0.049      0   4.96        0.348
#> 124 0.375 10.8  5.00   0.270    0.054      0   3.01        0.347
#> 125 0.453  8.6  3.00   0.200    0.040      0   2.61        0.347
#> 126 0.333  7.4  2.00   0.186    0.093      0   4.76        0.346
#> 127 0.313  5.4  1.50   0.136    0.091      0   3.73        0.346
#> 128 0.310  7.7  1.00   0.170    0.149      0   5.44        0.346
#> 129 0.333  5.7  3.00   0.143    0.038      0   4.60        0.346
#> 130 0.450  8.4  2.50   0.185    0.074      0   2.35        0.345
#> 131 0.316  7.4 13.00   0.197    0.015      0   4.16        0.345
#> 132 0.304  6.1  1.25   0.147    0.118      0   5.25        0.345
#> 133 0.378  6.5  1.33   0.157    0.098      0   3.04        0.345
#> 134 0.317  4.4  2.00   0.118    0.039      0   3.71        0.343
#> 135 0.364  7.2  2.00   0.174    0.087      0   4.03        0.343
#> 136 0.385  5.4  2.00   0.125    0.062      0   2.81        0.342
#> 137 0.302  7.0  1.91   0.168    0.088      0   5.06        0.342
#> 138 0.364  7.9  2.50   0.196    0.078      0   4.76        0.342
#> 139 0.321  6.2  3.67   0.156    0.043      0   4.13        0.342
#> 140 0.348 10.4 10.00   0.278    0.028      0   4.23        0.340
#> 141 0.286  8.6  1.75   0.212    0.121      0   4.68        0.340
#> 142 0.364 10.4  2.50   0.270    0.108      0   3.74        0.340
#> 143 0.290  9.0  2.20   0.224    0.102      0   4.86        0.339
#> 144 0.250  5.2  1.18   0.131    0.111      0   6.38        0.339
#> 145 0.360  7.7  5.50   0.206    0.028      0   3.93        0.339
#> 146 0.373  8.2  4.00   0.211    0.053      0   3.32        0.339
#> 147 0.360  9.3  4.50   0.231    0.051      0   3.99        0.338
#> 148 0.375  6.8  2.00   0.167    0.083      0   4.04        0.337
#> 149 0.318  9.3  3.00   0.250    0.083      0   3.99        0.336
#> 150 0.298  5.9  1.00   0.148    0.148      0   4.88        0.336
#> 151 0.278 10.6  1.80   0.257    0.143      0   5.36        0.335
#> 152 0.429  9.0    NA   0.222    0.000      0   1.13        0.335
#> 153 0.333  6.1  1.75   0.167    0.071      0   3.93        0.335
#> 154 0.371  7.6  1.50   0.176    0.098      0   3.13        0.335
#> 155 0.261  5.8  1.54   0.149    0.097      0   5.49        0.334
#> 156 0.292  5.6  2.50   0.156    0.062      0   4.26        0.334
#> 157 0.348 12.0  2.06   0.289    0.133      0   3.83        0.334
#> 158 0.143  8.1  1.00   0.200    0.200      0   8.30        0.334
#> 159 0.314  4.0  0.80   0.091    0.114      0   3.91        0.332
#> 160 0.267  5.8  1.50   0.143    0.095      0   6.23        0.332
#> 161 0.402  9.2  3.75   0.226    0.060      0   2.55        0.332
#> 162 0.313  7.2  1.33   0.174    0.130      0   3.33        0.330
#> 163 0.250 12.3  1.67   0.278    0.111      0   6.88        0.329
#> 164 0.357  7.2  2.00   0.190    0.095      0   2.73        0.329
#> 165 0.261  4.5  0.57   0.111    0.167      0   6.01        0.329
#> 166 0.261  3.9  1.00   0.097    0.097      0   5.42        0.328
#> 167 0.351  6.8  2.00   0.170    0.066      0   3.05        0.328
#> 168 0.355  8.7  5.00   0.213    0.021      0   3.03        0.327
#> 169 0.286  5.7  1.00   0.133    0.100      0   5.43        0.326
#> 170 0.375  9.5  2.00   0.231    0.115      0   3.13        0.325
#> 171 0.250  6.6  2.00   0.177    0.088      0   5.02        0.324
#> 172 0.344  6.2  1.82   0.163    0.089      0   3.34        0.324
#> 173 0.325 10.2  4.50   0.269    0.060      0   3.87        0.323
#> 174 0.319  9.0  5.00   0.236    0.038      0   3.69        0.323
#> 175 0.308  4.6  0.94   0.119    0.127      0   4.30        0.323
#> 176 0.250 10.4    NA   0.294    0.000      0   5.45        0.323
#> 177 0.250  9.0  5.00   0.263    0.053      0   4.33        0.323
#> 178 0.403 11.9  4.13   0.284    0.069      0   2.61        0.322
#> 179 0.267  8.2  2.90   0.212    0.073      0   4.88        0.322
#> 180 0.287  4.6  3.20   0.126    0.039      0   4.68        0.321
#> 181 0.313  9.4  3.88   0.246    0.063      0   4.06        0.321
#> 182 0.250  4.5  0.50   0.091    0.182      0   5.13        0.319
#> 183 0.337  7.2  2.56   0.190    0.074      0   3.38        0.318
#> 184 0.345  9.0  1.80   0.205    0.091      0   3.91        0.318
#> 185 0.289  4.0  1.22   0.101    0.073      0   4.83        0.318
#> 186 0.300  4.6  1.86   0.121    0.056      0   4.61        0.318
#> 187 0.313  8.8  3.25   0.215    0.066      0   4.05        0.318
#> 188 0.357 13.3  5.60   0.350    0.062      0   3.34        0.318
#> 189 0.329  8.8  3.25   0.224    0.069      0   3.55        0.317
#> 190 0.275  4.9  1.50   0.130    0.087      0   4.91        0.317
#> 191 0.371  7.9  2.86   0.204    0.061      0   2.28        0.317
#> 192 0.300  5.4  0.67   0.125    0.188      0   4.75        0.317
#> 193 0.308  6.9  3.33   0.187    0.056      0   3.40        0.316
#> 194 0.269  5.4  2.50   0.143    0.057      0   4.62        0.316
#> 195 0.384 12.4  6.14   0.333    0.054      0   2.81        0.316
#> 196 0.373 10.5  3.71   0.271    0.062      0   2.32        0.316
#> 197 0.321  7.6  1.29   0.205    0.159      0   3.43        0.315
#> 198 0.302  8.6  4.20   0.228    0.054      0   3.68        0.315
#> 199 0.351  6.8  2.67   0.167    0.062      0   2.45        0.313
#> 200 0.349  6.5  1.60   0.174    0.087      0   3.36        0.313
#> 201 0.338  7.7  3.33   0.213    0.053      0   3.18        0.311
#> 202 0.231  9.6  1.57   0.234    0.128      0   5.61        0.311
#> 203 0.407  6.8  0.78   0.152    0.065      0   2.91        0.311
#> 204 0.300  5.3  5.00   0.147    0.020      0   3.73        0.310
#> 205 0.299  7.9  2.22   0.200    0.090      0   3.54        0.310
#> 206 0.206  5.7  2.33   0.143    0.041      0   6.50        0.310
#> 207 0.262  5.2  2.17   0.144    0.067      0   4.54        0.310
#> 208 0.333  6.0  1.00   0.154    0.154      0   3.80        0.309
#> 209 0.125  7.7  3.00   0.231    0.077      0   5.99        0.308
#> 210 0.333  5.7  3.50   0.152    0.043      0   2.41        0.308
#> 211 0.197  7.9  1.56   0.208    0.133      0   5.59        0.308
#> 212 0.233  7.4  9.00   0.205    0.023      0   4.68        0.308
#> 213 0.256  6.4  0.75   0.148    0.180      0   5.43        0.308
#> 214 0.288  5.2  1.50   0.136    0.091      0   4.42        0.308
#> 215 0.344  5.9  3.00   0.154    0.051      0   3.17        0.308
#> 216 0.266  8.0  1.91   0.210    0.110      0   4.56        0.306
#> 217 0.667 21.6  4.00   0.500    0.125      0  -1.03        0.306
#> 218 0.375  9.0  5.00   0.227    0.045      0   1.73        0.306
#> 219 0.143  8.2  3.33   0.208    0.062      0   6.77        0.305
#> 220 0.311  8.3  4.75   0.218    0.046      0   3.28        0.305
#> 221 0.286  0.0    NA   0.000    0.000      0   3.13        0.305
#> 222 0.212  5.1  1.20   0.122    0.061      0   5.98        0.304
#> 223 0.316  7.5  5.20   0.197    0.038      0   3.49        0.303
#> 224 0.436 13.2  7.67   0.343    0.015      0   1.36        0.303
#> 225 0.311  7.0  3.29   0.183    0.048      0   3.96        0.303
#> 226 0.306 11.9  4.56   0.318    0.062      0   3.13        0.303
#> 227 0.353  5.1    NA   0.150    0.000      0   1.96        0.302
#> 228 0.379  9.6  1.83   0.239    0.109      0   2.44        0.302
#> 229 0.240  9.0  2.20   0.250    0.114      0   4.86        0.301
#> 230 0.306  8.0  2.75   0.212    0.077      0   3.38        0.301
#> 231 0.289  7.8  1.22   0.183    0.117      0   4.12        0.301
#> 232 0.296 11.4  4.00   0.314    0.078      0   3.87        0.301
#> 233 0.324  8.0  2.75   0.216    0.059      0   3.13        0.300
#> 234 0.211 11.9  2.20   0.297    0.135      0   5.48        0.300
#> 235 0.229  6.1  2.29   0.165    0.062      0   4.77        0.299
#> 236 0.301  5.7  2.86   0.161    0.056      0   3.46        0.298
#> 237 0.308  5.1  2.33   0.143    0.061      0   2.72        0.296
#> 238 0.358  7.5  5.67   0.193    0.023      0   2.39        0.296
#> 239 0.348  9.2 16.00   0.252    0.016      0   2.20        0.296
#> 240 0.355  6.0  2.00   0.150    0.050      0   2.47        0.295
#> 241 0.370  9.5  5.14   0.255    0.035      0   2.40        0.295
#> 242 0.192  9.3  2.00   0.250    0.109      0   5.36        0.293
#> 243 0.222  9.0  2.00   0.235    0.118      0   3.38        0.291
#> 244 0.303  4.4  5.33   0.123    0.023      0   2.82        0.290
#> 245 0.305  6.0  3.50   0.165    0.047      0   3.29        0.290
#> 246 0.293  3.8  5.00   0.100    0.020      0   3.63        0.289
#> 247 0.357  7.7  2.00   0.200    0.100      0   2.66        0.289
#> 248 0.294  6.4  8.00   0.182    0.000      0   2.86        0.289
#> 249 0.233  5.4  1.50   0.143    0.095      0   4.43        0.288
#> 250 0.333  7.4  1.50   0.176    0.118      0   3.13        0.288
#> 251 0.183  6.4  1.78   0.170    0.096      0   5.66        0.287
#> 252 0.238  9.3  3.00   0.250    0.056      0   4.84        0.286
#> 253 0.188  7.5  1.67   0.200    0.120      0   5.13        0.286
#> 254 0.333 12.6  4.67   0.333    0.071      0   2.53        0.284
#> 255 0.310  9.6  1.67   0.222    0.133      0   2.91        0.284
#> 256 0.412 16.2  5.00   0.395    0.079      0   1.28        0.284
#> 257 0.293  5.0  1.25   0.132    0.105      0   3.36        0.284
#> 258 0.273  7.9  3.50   0.219    0.062      0   3.76        0.284
#> 259 0.379  9.8  6.00   0.279    0.047      0   1.50        0.283
#> 260 0.232  5.6  1.67   0.153    0.092      0   4.47        0.283
#> 261 0.348  7.7    NA   0.207    0.000      0   1.42        0.282
#> 262 0.282  4.9  2.80   0.131    0.047      0   3.65        0.282
#> 263 0.250  4.3  0.67   0.105    0.132      0   4.00        0.282
#> 264 0.333  7.9  2.50   0.217    0.087      0   2.94        0.282
#> 265 0.250 12.7 15.00   0.375    0.025      0   4.31        0.282
#> 266 0.222  8.4  1.83   0.216    0.108      0   4.60        0.281
#> 267 0.235  8.6  1.17   0.233    0.200      0   3.70        0.280
#> 268 0.278  6.9 25.00   0.194    0.008      0   3.29        0.280
#> 269 0.308  8.4  2.90   0.220    0.068      0   2.65        0.280
#> 270 0.250  5.1    NA   0.150    0.000      0   4.51        0.279
#> 271 0.274  6.3  3.17   0.188    0.059      0   3.36        0.278
#> 272 0.367  8.4  1.50   0.200    0.089      0   2.48        0.276
#> 273 0.263 10.6  3.50   0.289    0.082      0   3.57        0.276
#> 274 0.214  6.8  1.17   0.167    0.119      0   4.67        0.275
#> 275 0.280  6.8  2.33   0.184    0.079      0   3.24        0.274
#> 276 0.286  5.9  6.00   0.167    0.028      0   3.10        0.274
#> 277 0.342  8.4  5.75   0.230    0.040      0   1.73        0.274
#> 278 0.364 15.6    NA   0.478    0.000      0   1.66        0.274
#> 279 0.319  7.2  2.86   0.198    0.059      0   2.89        0.273
#> 280 0.297  8.2 23.00   0.223    0.010      0   3.09        0.273
#> 281 0.368  9.5  2.33   0.241    0.069      0   1.84        0.273
#> 282 0.133  6.4  2.13   0.185    0.087      0   5.97        0.271
#> 283 0.308  7.2  2.00   0.205    0.077      0   2.73        0.271
#> 284 0.270  5.4  1.75   0.140    0.080      0   2.96        0.269
#> 285 0.192  6.0  3.00   0.162    0.027      0   5.36        0.269
#> 286 0.273  5.4  1.00   0.133    0.133      0   3.78        0.268
#> 287 0.228  7.1  1.92   0.193    0.084      0   4.03        0.268
#> 288 0.385 10.4  5.00   0.263    0.053      0   1.43        0.268
#> 289 0.188  7.9  5.00   0.217    0.043      0   4.29        0.267
#> 290 0.242  8.3  1.71   0.218    0.109      0   3.90        0.267
#> 291 0.273  7.3  2.00   0.212    0.094      0   3.45        0.266
#> 292 0.333  7.7  2.67   0.222    0.056      0   2.04        0.265
#> 293 0.375  7.7    NA   0.200    0.000      0   1.23        0.264
#> 294 0.271  9.9  4.33   0.277    0.053      0   3.22        0.264
#> 295 0.333  7.0    NA   0.194    0.000      0   1.88        0.263
#> 296 0.267 14.6  4.33   0.406    0.094      0   2.63        0.262
#> 297 0.300  8.1  9.00   0.225    0.025      0   1.63        0.262
#> 298 0.071  1.9  0.50   0.056    0.111      0   7.18        0.261
#> 299 0.250  2.6    NA   0.073    0.000      0   3.83        0.261
#> 300 0.278  6.4  1.33   0.160    0.080      0   2.75        0.261
#> 301 0.203  8.1  2.40   0.235    0.088      0   4.32        0.261
#> 302 0.234  7.0  1.40   0.192    0.137      0   3.97        0.260
#> 303 0.208  8.1  1.80   0.220    0.098      0   4.13        0.260
#> 304 0.333 10.8  2.50   0.278    0.111      0   2.15        0.258
#> 305 0.288 10.5 10.33   0.290    0.028      0   2.83        0.258
#> 306 0.000  6.8  0.25   0.125    0.375      0   9.50        0.258
#> 307 0.307  6.5  6.00   0.184    0.031      0   2.05        0.258
#> 308 0.182  7.2  1.33   0.211    0.053      0   4.73        0.257
#> 309 0.219  2.6  1.00   0.077    0.051      0   4.42        0.256
#> 310 0.217  6.8  6.00   0.194    0.032      0   3.63        0.255
#> 311 0.192  4.3  1.71   0.126    0.074      0   4.05        0.255
#> 312 0.235  4.5  2.75   0.126    0.046      0   3.68        0.255
#> 313 0.250  9.3  1.29   0.231    0.179      0   3.50        0.255
#> 314 0.264  6.4  3.25   0.181    0.056      0   3.08        0.254
#> 315 0.304 11.6  1.71   0.286    0.143      0   2.47        0.254
#> 316 0.244  5.5  2.67   0.150    0.056      0   3.71        0.254
#> 317 0.250  6.2  2.00   0.165    0.073      0   3.29        0.254
#> 318 0.250  3.8  1.38   0.105    0.076      0   3.44        0.253
#> 319 0.364 11.0  3.67   0.306    0.083      0   1.69        0.253
#> 320 0.231  9.3 11.00   0.275    0.025      0   3.82        0.253
#> 321 0.313  7.9  2.50   0.217    0.087      0   2.36        0.251
#> 322 0.214  5.9  2.33   0.179    0.077      0   3.92        0.251
#> 323 0.389 11.7  5.00   0.333    0.067      0   1.19        0.251
#> 324 0.304 11.2  4.00   0.300    0.075      0   1.83        0.251
#> 325 0.159  4.2  1.43   0.119    0.083      0   4.70        0.250
#> 326 0.000 11.6  0.75   0.273    0.364      0   5.99        0.250
#> 327 0.294  9.9  7.00   0.269    0.000      0   2.97        0.249
#> 328 0.176  7.7  3.00   0.231    0.077      0   4.13        0.248
#> 329 0.246 10.1  2.15   0.283    0.131      0   2.97        0.247
#> 330 0.273  8.0  2.00   0.235    0.118      0   2.69        0.247
#> 331 0.233  5.4  2.00   0.143    0.024      0   3.83        0.247
#> 332 0.222 11.9  5.50   0.324    0.059      0   3.50        0.247
#> 333 0.237  9.4  5.33   0.267    0.050      0   3.53        0.247
#> 334 0.273  9.2  2.57   0.243    0.081      0   3.02        0.246
#> 335 0.250  7.5  2.50   0.217    0.087      0   2.47        0.246
#> 336 0.286  7.7  2.00   0.200    0.100      0   2.66        0.245
#> 337 0.174  8.7  3.00   0.250    0.083      0   3.57        0.243
#> 338 0.333 11.7  2.60   0.283    0.043      0   1.13        0.242
#> 339 0.258  7.5  3.86   0.216    0.056      0   2.51        0.241
#> 340 0.254  5.8  1.71   0.154    0.090      0   2.97        0.241
#> 341 0.233  7.4  3.86   0.211    0.055      0   3.01        0.240
#> 342 0.179  6.3  2.50   0.185    0.052      0   4.16        0.240
#> 343 0.067  8.5  2.00   0.222    0.111      0   5.76        0.239
#> 344 0.200 10.8  2.80   0.304    0.109      0   3.40        0.239
#> 345 0.357  9.0    NA   0.250    0.000      0   1.13        0.239
#> 346 0.214  9.7  3.43   0.267    0.078      0   3.09        0.238
#> 347 0.182  8.3  4.00   0.235    0.059      0   3.87        0.237
#> 348 0.273  5.6  1.67   0.161    0.097      0   3.01        0.237
#> 349 0.243  4.6  1.50   0.128    0.085      0   3.13        0.235
#> 350 0.318 12.5  6.50   0.351    0.054      0   0.94        0.234
#> 351 0.269 11.0  4.33   0.302    0.070      0   1.76        0.234
#> 352 0.219  4.5  1.67   0.125    0.075      0   3.03        0.233
#> 353 0.263 12.5  2.60   0.351    0.135      0   1.93        0.232
#> 354 0.231  4.2  2.00   0.125    0.062      0   2.89        0.232
#> 355 0.280  8.3  8.00   0.222    0.028      0   1.91        0.231
#> 356 0.265  8.6  1.86   0.241    0.111      0   2.53        0.230
#> 357 0.000 15.4  1.33   0.444    0.333      0   3.61        0.229
#> 358 0.290 10.4  3.75   0.300    0.060      0   1.52        0.229
#> 359 0.280 10.3  4.38   0.289    0.066      0   2.14        0.228
#> 360 0.208  4.6  2.67   0.133    0.050      0   3.53        0.228
#> 361 0.207  9.7 14.00   0.304    0.022      0   3.21        0.228
#> 362 0.278  7.5  5.00   0.208    0.042      0   1.97        0.228
#> 363 0.235  9.0    NA   0.265    0.000      0   3.13        0.228
#> 364 0.143 12.3  2.50   0.345    0.138      0   3.84        0.227
#> 365 0.200  2.8  0.67   0.080    0.080      0   3.46        0.226
#> 366 0.236  8.4  3.57   0.238    0.067      0   2.52        0.226
#> 367 0.130  9.3  2.20   0.268    0.122      0   4.02        0.225
#> 368 0.250  9.3  6.00   0.275    0.046      0   2.20        0.225
#> 369 0.333  9.9  5.50   0.275    0.025      0   1.23        0.225
#> 370 0.231  8.4  1.67   0.238    0.143      0   2.94        0.224
#> 371 0.167  0.0  0.00   0.000    0.143      0   4.63        0.224
#> 372 0.176 11.1  2.50   0.312    0.125      0   3.34        0.223
#> 373 0.318 13.1  7.50   0.385    0.051      0   0.76        0.222
#> 374 0.286  4.5    NA   0.125    0.000      0   2.13        0.220
#> 375 0.176  6.3  1.27   0.182    0.143      0   3.53        0.220
#> 376 0.269 12.8  4.50   0.367    0.082      0   1.41        0.220
#> 377 0.128  4.5  1.40   0.132    0.075      0   4.85        0.220
#> 378 0.243  6.2  4.17   0.184    0.044      0   2.69        0.218
#> 379 0.188 11.0  1.80   0.300    0.133      0   2.29        0.217
#> 380 0.217 10.6  4.33   0.325    0.050      0   2.50        0.215
#> 381 0.269 10.5  3.00   0.286    0.071      0   1.65        0.214
#> 382 0.222  0.0    NA   0.000    0.000      0   3.13        0.214
#> 383 0.221  7.0  3.25   0.200    0.062      0   2.38        0.213
#> 384 0.278  7.9  5.00   0.208    0.042      0   1.79        0.212
#> 385 0.095  7.6  7.00   0.226    0.032      0   4.99        0.212
#> 386 0.385 13.5  2.67   0.333    0.042      0   0.58        0.212
#> 387 0.213  9.2  7.60   0.275    0.029      0   2.62        0.210
#> 388 0.263 10.8  2.00   0.294    0.118      0   2.15        0.210
#> 389 0.228  7.3  4.60   0.211    0.046      0   2.60        0.209
#> 390 0.219  9.9  3.33   0.283    0.085      0   2.51        0.208
#> 391 0.157  8.7  2.75   0.262    0.095      0   3.54        0.207
#> 392 0.182  6.8  1.00   0.176    0.118      0   3.13        0.207
#> 393 0.333 12.5 13.00   0.371    0.029      0   0.61        0.207
#> 394 0.216 10.3  6.17   0.306    0.050      0   2.39        0.206
#> 395 0.273  7.7  6.00   0.207    0.034      0   1.85        0.206
#> 396 0.167  5.0  1.67   0.147    0.059      0   4.13        0.205
#> 397 0.158  9.7  9.00   0.281    0.031      0   3.26        0.203
#> 398 0.253  9.1  7.25   0.257    0.035      0   1.61        0.203
#> 399 0.176  7.3  3.50   0.212    0.051      0   3.21        0.202
#> 400 0.250 10.8  3.00   0.300    0.100      0   1.93        0.201
#> 401 0.217  7.3  2.33   0.206    0.088      0   2.52        0.201
#> 402 0.207  7.4  2.25   0.214    0.095      0   2.59        0.200
#> 403 0.241 10.6  6.50   0.289    0.044      0   1.59        0.200
#> 404 0.192  6.0  6.00   0.182    0.030      0   2.13        0.199
#> 405 0.125  9.0  1.40   0.250    0.179      0   3.28        0.199
#> 406 0.267 13.5  3.67   0.379    0.069      0   0.88        0.195
#> 407 0.217 13.9  2.71   0.388    0.143      0   1.73        0.188
#> 408 0.150 11.0  4.33   0.342    0.079      0   2.74        0.188
#> 409 0.222  8.1  3.00   0.231    0.077      0   2.17        0.188
#> 410 0.167 12.0  2.00   0.333    0.167      0   2.47        0.188
#> 411 0.182  7.9  2.50   0.263    0.105      0   2.36        0.185
#> 412 0.227 10.0  3.33   0.286    0.086      0   1.91        0.185
#> 413 0.278 16.0  6.33   0.463    0.073      0   0.58        0.184
#> 414 0.143  6.4  1.33   0.190    0.143      0   3.33        0.182
#> 415 0.111 11.4 11.00   0.355    0.032      0   2.40        0.181
#> 416 0.208  9.0  1.67   0.250    0.075      0   2.03        0.180
#> 417 0.138  5.9  3.50   0.175    0.050      0   3.62        0.174
#> 418 0.111  4.7  2.50   0.143    0.057      0   4.11        0.174
#> 419 0.050  6.8  2.33   0.212    0.091      0   4.34        0.173
#> 420 0.200  6.5  8.00   0.205    0.026      0   1.95        0.172
#> 421 0.000  7.7  1.00   0.250    0.250      0   4.09        0.172
#> 422 0.172  7.1  3.00   0.205    0.068      0   2.59        0.172
#> 423 0.158  5.4  2.00   0.163    0.081      0   2.84        0.169
#> 424 0.176 15.9  5.00   0.465    0.093      0   1.15        0.167
#> 425 0.250  8.3    NA   0.250    0.000      0   1.18        0.165
#> 426 0.167 13.0  2.80   0.378    0.135      0   1.72        0.164
#> 427 0.208 10.2 11.00   0.306    0.028      0   1.07        0.162
#> 428 0.150  4.5  3.00   0.120    0.040      0   3.13        0.162
#> 429 0.056  9.0  1.50   0.273    0.182      0   3.13        0.152
#> 430 0.208  8.0    NA   0.250    0.000      0   1.36        0.149
#> 431 0.158 11.3  6.50   0.371    0.057      0   1.45        0.135
#> 432 0.130  9.9  3.67   0.289    0.053      0   1.83        0.134
#> 433 0.167  4.5    NA   0.143    0.000      0   2.13        0.126
#> 434 0.083 10.5  3.50   0.326    0.093      0   2.05        0.122
#> 435 0.273 19.1 17.00   0.586    0.034      1  -0.74        0.115
#> 436 0.190 14.7  6.33   0.442    0.047      0   0.28        0.114
#> 437 0.105 10.6  5.50   0.344    0.062      0   1.38        0.110
#> 438 0.115  6.8  7.00   0.206    0.029      0   1.93        0.098
#> 439 0.118  1.6    NA   0.056    0.000      0   2.75        0.098
#> 440 0.105 10.0 10.00   0.333    0.033      0   1.25        0.082
#> 441 0.077  5.4    NA   0.188    0.000      0   1.93        0.078
#> 442 0.000  6.8  3.00   0.231    0.077      0   2.38        0.053
#> 443 0.077  8.5    NA   0.316    0.000      0   1.17        0.046
#> 444 0.042  4.7  5.00   0.167    0.000      0   2.05        0.042
#> 445 0.000  7.2    NA   0.267    0.000      0   1.53        0.000
#> 446 0.000  0.0    NA   0.000    0.000      0   3.13        0.000
#> 447 0.000  9.0    NA   0.333    0.000      0   1.13        0.000
#> 448 0.000  0.0    NA   0.000    0.000      0   3.13        0.000
#> 449 0.000  9.0    NA   0.333    0.000      0   1.13        0.000
#> 450 0.000  0.0    NA   0.000    0.000      0   3.13        0.000
#> 451 0.000 13.5    NA   0.500    0.000      0  -6.87        0.000
#> 452    NA 27.0    NA   1.000    0.000      1 -16.87        0.000
#> 453 0.000  0.0    NA   0.000    0.000      0   3.13        0.000
#>     wOBA_CON_against
#> 1              1.133
#> 2              0.736
#> 3                NaN
#> 4              0.797
#> 5              0.734
#> 6              0.765
#> 7              0.971
#> 8              0.736
#> 9              0.679
#> 10             0.754
#> 11             0.745
#> 12             0.702
#> 13             0.712
#> 14             0.604
#> 15             0.602
#> 16             0.668
#> 17             0.586
#> 18             0.610
#> 19             0.482
#> 20             0.570
#> 21             0.533
#> 22             0.506
#> 23             0.534
#> 24             0.883
#> 25             0.525
#> 26             0.594
#> 27             0.543
#> 28             0.502
#> 29             0.519
#> 30             0.604
#> 31             0.554
#> 32             0.634
#> 33             0.619
#> 34             0.470
#> 35             0.500
#> 36             0.661
#> 37             0.534
#> 38             0.421
#> 39             0.491
#> 40             0.548
#> 41             0.471
#> 42             0.413
#> 43             0.617
#> 44             0.508
#> 45             0.507
#> 46             0.426
#> 47             0.401
#> 48             0.452
#> 49             0.558
#> 50             0.466
#> 51             0.484
#> 52             0.410
#> 53             0.511
#> 54             0.408
#> 55             0.455
#> 56             0.540
#> 57             0.699
#> 58             0.458
#> 59             0.435
#> 60             0.432
#> 61             0.462
#> 62             0.551
#> 63             0.500
#> 64             0.434
#> 65             0.474
#> 66             0.431
#> 67             0.478
#> 68             0.398
#> 69             0.391
#> 70             0.435
#> 71             0.407
#> 72             0.409
#> 73             0.438
#> 74             0.452
#> 75             0.446
#> 76             0.540
#> 77             0.495
#> 78             0.358
#> 79             0.422
#> 80             0.503
#> 81             0.493
#> 82             0.513
#> 83             0.375
#> 84             0.426
#> 85             0.451
#> 86             0.456
#> 87             0.383
#> 88             0.410
#> 89             0.377
#> 90             0.452
#> 91             0.579
#> 92             0.443
#> 93             0.515
#> 94             0.515
#> 95             0.425
#> 96             0.454
#> 97             0.280
#> 98             0.378
#> 99             0.450
#> 100            0.427
#> 101            0.428
#> 102            0.389
#> 103            0.402
#> 104            0.379
#> 105            0.418
#> 106            0.471
#> 107            0.340
#> 108            0.402
#> 109            0.428
#> 110            0.478
#> 111            0.473
#> 112            0.456
#> 113            0.372
#> 114            0.387
#> 115            0.346
#> 116            0.385
#> 117            0.389
#> 118            0.405
#> 119            0.294
#> 120            0.353
#> 121            0.456
#> 122            0.375
#> 123            0.368
#> 124            0.458
#> 125            0.444
#> 126            0.382
#> 127            0.369
#> 128            0.370
#> 129            0.395
#> 130            0.417
#> 131            0.433
#> 132            0.408
#> 133            0.383
#> 134            0.393
#> 135            0.383
#> 136            0.368
#> 137            0.371
#> 138            0.420
#> 139            0.392
#> 140            0.462
#> 141            0.385
#> 142            0.427
#> 143            0.412
#> 144            0.357
#> 145            0.435
#> 146            0.406
#> 147            0.399
#> 148            0.330
#> 149            0.424
#> 150            0.325
#> 151            0.360
#> 152            0.503
#> 153            0.387
#> 154            0.370
#> 155            0.358
#> 156            0.378
#> 157            0.421
#> 158            0.368
#> 159            0.350
#> 160            0.349
#> 161            0.401
#> 162            0.346
#> 163            0.425
#> 164            0.395
#> 165            0.336
#> 166            0.352
#> 167            0.384
#> 168            0.451
#> 169            0.368
#> 170            0.354
#> 171            0.345
#> 172            0.355
#> 173            0.418
#> 174            0.420
#> 175            0.318
#> 176            0.477
#> 177            0.420
#> 178            0.433
#> 179            0.376
#> 180            0.352
#> 181            0.416
#> 182            0.267
#> 183            0.371
#> 184            0.375
#> 185            0.352
#> 186            0.343
#> 187            0.384
#> 188            0.456
#> 189            0.382
#> 190            0.329
#> 191            0.380
#> 192            0.335
#> 193            0.363
#> 194            0.345
#> 195            0.457
#> 196            0.425
#> 197            0.323
#> 198            0.387
#> 199            0.360
#> 200            0.370
#> 201            0.396
#> 202            0.350
#> 203            0.427
#> 204            0.369
#> 205            0.338
#> 206            0.343
#> 207            0.349
#> 208            0.294
#> 209            0.369
#> 210            0.355
#> 211            0.331
#> 212            0.357
#> 213            0.281
#> 214            0.318
#> 215            0.335
#> 216            0.333
#> 217            0.587
#> 218            0.377
#> 219            0.314
#> 220            0.366
#> 221            0.356
#> 222            0.326
#> 223            0.352
#> 224            0.484
#> 225            0.370
#> 226            0.422
#> 227            0.355
#> 228            0.360
#> 229            0.363
#> 230            0.348
#> 231            0.349
#> 232            0.425
#> 233            0.379
#> 234            0.364
#> 235            0.341
#> 236            0.334
#> 237            0.319
#> 238            0.369
#> 239            0.399
#> 240            0.348
#> 241            0.391
#> 242            0.349
#> 243            0.317
#> 244            0.330
#> 245            0.329
#> 246            0.328
#> 247            0.339
#> 248            0.374
#> 249            0.302
#> 250            0.320
#> 251            0.296
#> 252            0.405
#> 253            0.299
#> 254            0.395
#> 255            0.299
#> 256            0.429
#> 257            0.278
#> 258            0.336
#> 259            0.372
#> 260            0.273
#> 261            0.389
#> 262            0.311
#> 263            0.260
#> 264            0.294
#> 265            0.461
#> 266            0.302
#> 267            0.251
#> 268            0.357
#> 269            0.334
#> 270            0.329
#> 271            0.323
#> 272            0.323
#> 273            0.348
#> 274            0.279
#> 275            0.277
#> 276            0.320
#> 277            0.337
#> 278            0.573
#> 279            0.311
#> 280            0.356
#> 281            0.344
#> 282            0.295
#> 283            0.300
#> 284            0.289
#> 285            0.316
#> 286            0.240
#> 287            0.296
#> 288            0.367
#> 289            0.321
#> 290            0.299
#> 291            0.280
#> 292            0.341
#> 293            0.330
#> 294            0.345
#> 295            0.309
#> 296            0.396
#> 297            0.327
#> 298            0.221
#> 299            0.289
#> 300            0.286
#> 301            0.301
#> 302            0.253
#> 303            0.287
#> 304            0.312
#> 305            0.349
#> 306            0.000
#> 307            0.310
#> 308            0.350
#> 309            0.261
#> 310            0.314
#> 311            0.259
#> 312            0.254
#> 313            0.271
#> 314            0.287
#> 315            0.284
#> 316            0.280
#> 317            0.269
#> 318            0.236
#> 319            0.320
#> 320            0.336
#> 321            0.275
#> 322            0.266
#> 323            0.343
#> 324            0.330
#> 325            0.227
#> 326            0.000
#> 327            0.359
#> 328            0.282
#> 329            0.268
#> 330            0.270
#> 331            0.290
#> 332            0.310
#> 333            0.309
#> 334            0.297
#> 335            0.267
#> 336            0.252
#> 337            0.278
#> 338            0.362
#> 339            0.281
#> 340            0.241
#> 341            0.274
#> 342            0.281
#> 343            0.184
#> 344            0.263
#> 345            0.341
#> 346            0.296
#> 347            0.259
#> 348            0.240
#> 349            0.224
#> 350            0.331
#> 351            0.280
#> 352            0.227
#> 353            0.271
#> 354            0.252
#> 355            0.277
#> 356            0.252
#> 357            0.000
#> 358            0.313
#> 359            0.280
#> 360            0.242
#> 361            0.316
#> 362            0.266
#> 363            0.310
#> 364            0.255
#> 365            0.214
#> 366            0.259
#> 367            0.212
#> 368            0.272
#> 369            0.308
#> 370            0.203
#> 371            0.147
#> 372            0.235
#> 373            0.331
#> 374            0.252
#> 375            0.170
#> 376            0.292
#> 377            0.228
#> 378            0.239
#> 379            0.236
#> 380            0.314
#> 381            0.277
#> 382            0.237
#> 383            0.228
#> 384            0.245
#> 385            0.256
#> 386            0.339
#> 387            0.276
#> 388            0.232
#> 389            0.236
#> 390            0.233
#> 391            0.211
#> 392            0.214
#> 393            0.312
#> 394            0.255
#> 395            0.240
#> 396            0.224
#> 397            0.254
#> 398            0.246
#> 399            0.226
#> 400            0.220
#> 401            0.208
#> 402            0.195
#> 403            0.239
#> 404            0.235
#> 405            0.134
#> 406            0.285
#> 407            0.192
#> 408            0.242
#> 409            0.196
#> 410            0.147
#> 411            0.194
#> 412            0.200
#> 413            0.266
#> 414            0.126
#> 415            0.259
#> 416            0.215
#> 417            0.186
#> 418            0.168
#> 419            0.147
#> 420            0.201
#> 421            0.000
#> 422            0.165
#> 423            0.151
#> 424            0.178
#> 425            0.220
#> 426            0.147
#> 427            0.224
#> 428            0.132
#> 429            0.052
#> 430            0.199
#> 431            0.139
#> 432            0.131
#> 433            0.147
#> 434            0.073
#> 435            0.240
#> 436            0.168
#> 437            0.112
#> 438            0.106
#> 439            0.104
#> 440            0.093
#> 441            0.097
#> 442            0.000
#> 443            0.068
#> 444            0.052
#> 445            0.000
#> 446            0.000
#> 447            0.000
#> 448            0.000
#> 449            0.000
#> 450            0.000
#> 451            0.000
#> 452              NaN
#> 453            0.000
# }
```
