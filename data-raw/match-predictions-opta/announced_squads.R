# announced_squads.R
# Master list of officially-announced WC 2026 26-man squads + builder that
# resolves player names to Opta player_ids and emits a parquet consumed by
# the predictions pipeline (step 02) and the cross-check diagnostic
# (debug/wc2026_squad_crosscheck.R).
#
# Output:
#   data-raw/cache-predictions-opta/wc2026_announced_squads.parquet
#     columns: team_name, team_id, player_id, player_name, position,
#              expected_minutes_norm, is_starter_pred
#
# Source it stand-alone to (re)build the parquet:
#   cd panna && Rscript data-raw/match-predictions-opta/announced_squads.R
#
# Keys in WC2026_ANNOUNCED_SQUADS must match Opta `team_name` exactly
# (e.g. "United States", not "USA"; "Korea Republic", not "South Korea").
# As of 2026-05-27, 17 of 48 teams have published a final 26.

# 1. Master squad list ----

WC2026_ANNOUNCED_SQUADS <- list(

  # ----- Announcement wave 1 (pre 2026-05-12): existing in cross-check -----

  France = c(
    "Mike Maignan", "Robin Risser", "Brice Samba",
    "Lucas Digne", "Malo Gusto", "Lucas Hernandez", "Theo Hernandez",
    "Ibrahima Konate", "Maxence Lacroix", "Jules Kounde", "William Saliba",
    "Dayot Upamecano",
    "N'Golo Kante", "Manu Kone", "Adrien Rabiot", "Aurelien Tchouameni",
    "Warren Zaire-Emery",
    "Maghnes Akliouche", "Bradley Barcola", "Rayan Cherki",
    "Ousmane Dembele", "Desire Doue", "Michael Olise", "Kylian Mbappe",
    "Jean-Philippe Mateta", "Marcus Thuram"),

  Belgium = c(
    "Thibaut Courtois", "Senne Lammens", "Mike Penders",
    "Timothy Castagne", "Zeno Debast", "Maxim De Cuyper", "Koni De Winter",
    "Brandon Mechele", "Thomas Meunier", "Nathan Ngoy", "Joaquin Seys",
    "Arthur Theate",
    "Kevin De Bruyne", "Amadou Onana", "Nicolas Raskin", "Youri Tielemans",
    "Hans Vanaken", "Axel Witsel",
    "Charles De Ketelaere", "Jeremy Doku", "Matias Fernandez-Pardo",
    "Romelu Lukaku", "Dodi Lukebakio", "Diego Moreira",
    "Alexis Saelemaekers", "Leandro Trossard"),

  `Bosnia-Herzegovina` = c(
    "Nikola Vasilj", "Martin Zlomislic", "Osman Hadzikic",
    "Sead Kolasinac", "Amar Dedic", "Nihad Mujakic", "Nikola Katic",
    "Tarik Muharemovic", "Stjepan Radeljic", "Dennis Hadzikadunic",
    "Nidal Celik", "Amir Hadziahmetovic", "Ivan Sunjic", "Ivan Basic",
    "Dzenis Burnic", "Ermin Mahmic", "Benjamin Tahirovic", "Amar Memic",
    "Armin Gigovic", "Kerim Alajbegovic", "Esmir Bajraktarevic",
    "Ermedin Demirovic", "Jovo Lukic", "Samed Bazdar", "Haris Tabakovic",
    "Edin Dzeko"),

  `New Zealand` = c(
    "Max Crocombe", "Alex Paulsen", "Michael Woud",
    "Tim Payne", "Francis De Vries", "Tyler Bindon", "Michael Boxall",
    "Liberato Cacace", "Nando Pijnaker", "Finn Surman", "Callan Elliot",
    "Tommy Smith",
    "Joe Bell", "Matt Garbett", "Marko Stamenic", "Sarpreet Singh",
    "Alex Rufer", "Ryan Thomas",
    "Chris Wood", "Eli Just", "Kosta Barbarouses", "Ben Waine", "Ben Old",
    "Callum McCowatt", "Jesse Randall", "Lachlan Bayliss"),

  Japan = c(
    "Zion Suzuki", "Keisuke Osako", "Tomoki Hayakawa",
    "Yuto Nagatomo", "Shogo Taniguchi", "Ko Itakura", "Tsuyoshi Watanabe",
    "Takehiro Tomiyasu", "Hiroki Ito", "Ayumu Seko", "Yukinari Sugawara",
    "Junnosuke Suzuki",
    "Wataru Endo", "Junya Ito", "Daichi Kamada", "Ritsu Doan", "Ao Tanaka",
    "Keito Nakamura", "Kaishu Sano",
    "Takefusa Kubo", "Yuito Suzuki", "Koki Ogawa", "Daizen Maeda",
    "Ayase Ueda", "Kento Shiogai", "Keisuke Goto"),

  Tunisia = c(
    "Aymen Dahmen", "Sabri Ben Hessen", "Abdelmouhib Chamakh",
    "Montassar Talbi", "Dylan Bronn", "Omar Rekik", "Yan Valery", "Ali Abdi",
    "Moutaz Neffati", "Raed Chikhaoui", "Adam Arous",
    "Mohamed Amine Ben Hamida",
    "Ellyes Skhiri", "Hannibal Mejbri", "Anis Ben Slimane", "Hadj Mahmoud",
    "Rani Khedira", "Mortadha Ben Ouanes",
    "Elias Achouri", "Ismael Gharbi", "Elias Saad", "Sebastian Tounekti",
    "Firas Chaouat", "Khalil Ayari", "Hazem Mastouri", "Rayan Elloumi"),

  `Korea Republic` = c(
    "Jo Hyeon-woo", "Kim Seung-gyu", "Song Bum-keun",
    "Kim Min-jae", "Cho Yu-min", "Lee Han-beom", "Kim Tae-hyeon",
    "Park Jin-seob", "Lee Gi-hyuk", "Lee Tae-seok", "Seol Young-woo",
    "Jens Castrop", "Kim Moon-hwan",
    "Yang Hyun-jun", "Paik Seung-ho", "Hwang In-beom", "Kim Jin-gyu",
    "Bae Jun-ho", "Eom Ji-sung", "Hwang Hee-chan", "Lee Dong-gyeong",
    "Lee Jae-sung", "Lee Kang-in",
    "Oh Hyeon-gyu", "Son Heung-min", "Cho Gue-sung"),

  `Côte d'Ivoire` = c(
    "Yahia Fofana", "Mohamed Kone", "Alban Lafont",
    "Emmanuel Agbadou", "Clement Akpa", "Ousmane Diomande", "Guela Doue",
    "Ghislain Konan", "Odilon Kossounou", "Evan Ndicka", "Wilfried Singo",
    "Seko Fofana", "Parfait Guiagon", "Franck Kessie", "Christ Inao Oulai",
    "Ibrahim Sangare", "Jean Michael Seri",
    "Simon Adingra", "Ange-Yoan Bonny", "Amad Diallo", "Oumar Diakite",
    "Yan Diomande", "Evann Guessand", "Nicolas Pepe", "Bazoumana Toure",
    "Elye Wahi"),

  # ----- Announcement wave 2 (2026-05-18 to 2026-05-27) -----

  Brazil = c(
    "Alisson", "Ederson", "Weverton",
    "Alex Sandro", "Bremer", "Danilo", "Douglas Santos",
    "Gabriel Magalhaes", "Leo Pereira", "Marquinhos", "Roger Ibanez", "Wesley",
    "Bruno Guimaraes", "Casemiro", "Danilo Santos", "Fabinho",
    "Lucas Paqueta",
    "Endrick", "Gabriel Martinelli", "Igor Thiago", "Luiz Henrique",
    "Matheus Cunha", "Neymar", "Raphinha", "Rayan", "Vinicius Junior"),

  Scotland = c(
    "Craig Gordon", "Angus Gunn", "Liam Kelly",
    "Grant Hanley", "Jack Hendry", "Aaron Hickey", "Dom Hyam",
    "Scott McKenna", "Nathan Patterson", "Anthony Ralston", "Andy Robertson",
    "John Souttar", "Kieran Tierney",
    "Ryan Christie", "Finlay Curtis", "Lewis Ferguson", "Ben Gannon-Doak",
    "Billy Gilmour", "John McGinn", "Kenny McLean", "Scott McTominay",
    "Che Adams", "Lyndon Dykes", "George Hirst", "Lawrence Shankland",
    "Ross Stewart"),

  Switzerland = c(
    "Gregor Kobel", "Yvon Mvogo", "Marvin Keller",
    "Manuel Akanji", "Nico Elvedi", "Ricardo Rodriguez", "Silvan Widmer",
    "Miro Muheim", "Aurele Amenda", "Eray Comert", "Luca Jaquez",
    "Granit Xhaka", "Johan Manzambi", "Remo Freuler", "Denis Zakaria",
    "Ardon Jashari", "Djibril Sow", "Christian Fassnacht", "Michel Aebischer",
    "Fabian Rieder",
    "Ruben Vargas", "Breel Embolo", "Noah Okafor", "Dan Ndoye",
    "Zeki Amdouni", "Cedric Itten"),

  Germany = c(
    # Cross-checked against Wikipedia 2026-05-28: the 26th is Nadiem Amiri,
    # not Tim Kleindienst (Kleindienst was a placeholder I added when an
    # ESPN search result truncated to 25 names).
    "Oliver Baumann", "Manuel Neuer", "Alexander Nubel",
    "Waldemar Anton", "Nathaniel Brown", "David Raum", "Antonio Rudiger",
    "Nico Schlotterbeck", "Jonathan Tah", "Malick Thiaw",
    "Pascal Gross", "Joshua Kimmich", "Felix Nmecha", "Aleksandar Pavlovic",
    "Angelo Stiller", "Leon Goretzka", "Florian Wirtz", "Jamie Leweling",
    "Nadiem Amiri",
    "Maximilian Beier", "Kai Havertz", "Lennart Karl", "Jamal Musiala",
    "Leroy Sane", "Deniz Undav", "Nick Woltemade"),

  Spain = c(
    "Unai Simon", "David Raya", "Joan Garcia",
    "Marc Cucurella", "Pau Cubarsi", "Aymeric Laporte", "Alex Grimaldo",
    "Pedro Porro", "Eric Garcia", "Marcos Llorente", "Marc Pubill",
    "Gavi", "Rodri", "Pedri", "Martin Zubimendi", "Fabian Ruiz",
    "Alex Baena", "Mikel Merino",
    "Lamine Yamal", "Nico Williams", "Dani Olmo", "Ferran Torres",
    "Mikel Oyarzabal", "Yeremy Pino", "Borja Iglesias", "Victor Munoz"),

  `United States` = c(
    "Chris Brady", "Matt Freese", "Matt Turner",
    "Max Arfsten", "Sergino Dest", "Alex Freeman", "Mark McKenzie",
    "Tim Ream", "Chris Richards", "Antonee Robinson", "Miles Robinson",
    "Joe Scally", "Auston Trusty",
    "Tyler Adams", "Sebastian Berhalter", "Weston McKennie", "Cristian Roldan",
    "Brenden Aaronson", "Malik Tillman", "Tim Weah", "Alejandro Zendejas",
    "Christian Pulisic", "Gio Reyna",
    "Folarin Balogun", "Ricardo Pepi", "Haji Wright"),

  Morocco = c(
    "Yassine Bounou", "Munir El Kajoui", "Reda Tagnaouti",
    "Noussair Mazraoui", "Anass Salah-Eddine", "Youssef Belammari",
    "Achraf Hakimi", "Zakaria El Ouahdi", "Chadi Riad", "Nayef Aguerd",
    "Redouane Halhal", "Issa Diop", "Samir El Mourabet",
    "Ayyoub Bouaddi", "Neil El Aynaoui", "Sofyan Amrabat", "Azzedine Ounahi",
    "Bilal El Khannouss", "Ismael Saibari",
    "Abde Ezzalzouli", "Chemsdine Talbi", "Soufiane Rahimi", "Ayoub El Kaabi",
    "Brahim Diaz", "Gessime Yassine", "Ayoube Amaimouni"),

  Colombia = c(
    "David Ospina", "Alvaro Montero", "Camilo Vargas",
    "Daniel Munoz", "Jhon Lucumi", "Santiago Arias", "Davinson Sanchez",
    "Johan Mojica", "Yerry Mina", "Willer Ditta", "Deiver Machado",
    "Jorge Carrascal", "Kevin Castano", "Gustavo Puerta",
    "Juan Fernando Quintero", "Juan Portilla", "Jefferson Lerma",
    "Richard Rios", "Jhon Arias", "James Rodriguez", "Jaminton Campaz",
    "Luis Diaz", "Jhon Cordoba", "Luis Suarez", "Andres Gomez",
    "Cucho Hernandez"),

  Netherlands = c(
    "Mark Flekken", "Robin Roefs", "Bart Verbruggen",
    "Nathan Ake", "Denzel Dumfries", "Jorrel Hato", "Jurrien Timber",
    "Micky van de Ven", "Virgil van Dijk", "Jan Paul van Hecke",
    "Mats Wieffer", "Frenkie de Jong", "Marten de Roon", "Ryan Gravenberch",
    "Justin Kluivert", "Teun Koopmeiners", "Tijjani Reijnders", "Guus Til",
    "Quinten Timber",
    "Brian Brobbey", "Memphis Depay", "Cody Gakpo", "Noa Lang", "Donyell Malen",
    "Crysencio Summerville", "Wout Weghorst"),

  # ----- Announcement wave 3 (added 2026-05-28, from Wikipedia / ESPN agg) -----

  England = c(
    "Jordan Pickford", "Dean Henderson", "James Trafford",
    "Reece James", "Djed Spence", "Nico O'Reilly", "Marc Guehi",
    "John Stones", "Ezri Konsa", "Dan Burn", "Jarell Quansah",
    "Tino Livramento",
    "Declan Rice", "Jude Bellingham", "Elliot Anderson", "Kobbie Mainoo",
    "Jordan Henderson", "Eberechi Eze", "Morgan Rogers",
    "Bukayo Saka", "Marcus Rashford", "Harry Kane", "Ollie Watkins",
    "Anthony Gordon", "Noni Madueke", "Ivan Toney"),

  Norway = c(
    "Orjan Nyland", "Egil Selvik", "Sander Tangvik",
    "Julian Ryerson", "Kristoffer Ajer", "Leo Ostigard",
    "David Moller Wolfe", "Marcus Pedersen", "Torbjorn Heggem",
    "Fredrik Andre Bjorkan", "Henrik Falchener", "Sondre Langas",
    "Martin Odegaard", "Sander Berge", "Patrick Berg", "Kristian Thorstvedt",
    "Morten Thorsby", "Thelo Aasgaard",
    "Andreas Schjelderup", "Jens Petter Hauge", "Fredrik Aursnes",
    "Oscar Bobb", "Antonio Nusa", "Erling Haaland", "Alexander Sorloth",
    "Jorgen Strand Larsen"),

  Sweden = c(
    "Viktor Johansson", "Kristoffer Nordfeldt", "Jacob Widell Zetterstrom",
    "Hjalmar Ekdal", "Gabriel Gudmundsson", "Isak Hien", "Emil Holm",
    "Gustaf Lagerbielke", "Victor Lindelof", "Erik Smith", "Carl Starfelt",
    "Elliot Stroud", "Daniel Svensson",
    "Taha Ali", "Yasin Ayari", "Lucas Bergvall", "Jesper Karlstrom",
    "Ken Sema", "Mattias Svanberg", "Besfort Zeneli",
    "Alexander Bernhardsson", "Anthony Elanga", "Viktor Gyokeres",
    "Alexander Isak", "Gustaf Nilsson", "Benjamin Nygren"),

  Senegal = c(
    # Wikipedia shows 28 (Senegal trims to 26 by FIFA's June 2 deadline).
    # Kept as 26 here — the 2 likely-cut names (Bamba Dieng, Cherif Ndiaye)
    # are the two low-EM forwards crowding an already deep front three.
    "Edouard Mendy", "Mory Diaw", "Yehvann Diouf",
    "Krepin Diatta", "Antoine Mendy", "Kalidou Koulibaly",
    "El Hadji Malick Diouf", "Mamadou Sarr", "Moussa Niakhate",
    "Moustapha Mbow", "Abdoulaye Seck", "Ismail Jakobs", "Ilay Camara",
    "Idrissa Gana Gueye", "Pape Gueye", "Lamine Camara", "Habib Diarra",
    "Pathe Ciss", "Pape Matar Sarr", "Bara Sapoko Ndiaye",
    "Sadio Mane", "Ismaila Sarr", "Iliman Ndiaye", "Assane Diao",
    "Ibrahim Mbaye", "Nicolas Jackson"),

  # ----- Announcement wave 4 (cross-checked vs Wikipedia 2026-05-28) -----
  # 8 finalized squads Wikipedia had that I missed in earlier ESPN passes.

  Portugal = c(
    "Diogo Costa", "Jose Sa", "Rui Silva",
    "Ruben Dias", "Joao Cancelo", "Nelson Semedo", "Nuno Mendes",
    "Diogo Dalot", "Goncalo Inacio", "Matheus Nunes", "Renato Veiga",
    "Tomas Araujo",
    "Bernardo Silva", "Bruno Fernandes", "Ruben Neves", "Vitinha",
    "Joao Neves", "Samu Costa",
    "Cristiano Ronaldo", "Joao Felix", "Rafael Leao", "Goncalo Guedes",
    "Goncalo Ramos", "Pedro Neto", "Francisco Trincao",
    "Francisco Conceicao"),

  Croatia = c(
    "Dominik Livakovic", "Dominik Kotarski", "Ivor Pandur",
    "Josko Gvardiol", "Duje Caleta-Car", "Josip Sutalo", "Josip Stanisic",
    "Marin Pongracic", "Martin Erlic", "Luka Vuskovic",
    "Luka Modric", "Mateo Kovacic", "Mario Pasalic", "Nikola Vlasic",
    "Luka Sucic", "Martin Baturina", "Kristijan Jakic", "Petar Sucic",
    "Nikola Moro", "Toni Fruk",
    "Ivan Perisic", "Andrej Kramaric", "Ante Budimir", "Marco Pasalic",
    "Petar Musa", "Igor Matanovic"),

  Austria = c(
    "Alexander Schlager", "Patrick Pentz", "Florian Wiegele",
    "David Affengruber", "Kevin Danso", "Stefan Posch", "Philipp Lienhart",
    "Phillipp Mwene", "Marco Friedl", "Michael Svoboda",
    "Xaver Schlager", "Nicolas Seiwald", "Marcel Sabitzer",
    "Florian Grillitsch", "Carney Chukwuemeka", "Romano Schmid",
    "Christoph Baumgartner", "Konrad Laimer", "Patrick Wimmer",
    "Alexander Prass", "Paul Wanner",
    "Marko Arnautovic", "David Alaba", "Michael Gregoritsch",
    "Sasa Kalajdzic", "Alessandro Schopf"),

  Panama = c(
    "Luis Mejia", "Orlando Mosquera", "Cesar Samudio",
    "Eric Davis", "Fidel Escobar", "Michael Amir Murillo",
    "Roderick Miller", "Andres Andrade", "Cesar Blackman", "Jose Cordoba",
    "Jiovany Ramos", "Jorge Gutierrez",
    "Edgardo Farina", "Anibal Godoy", "Alberto Quintero", "Yoel Barcenas",
    "Adalberto Carrasquilla", "Jose Luis Rodriguez", "Cristian Martinez",
    "Cesar Yanis", "Carlos Harvey",
    "Azarias Londono", "Jose Fajardo", "Ismael Diaz", "Cecilio Waterman",
    "Tomas Rodriguez"),

  Haiti = c(
    "Johny Placide", "Alexandre Pierre", "Josue Duverger",
    "Ricardo Ade", "Carlens Arcus", "Martin Experience",
    "Jean-Kevin Duverne", "Duke Lacroix", "Wilguens Paugain",
    "Hannes Delcroix", "Keeto Thermoncy", "Leverton Pierre",
    "Danley Jean Jacques", "Carl Sainte", "Jean-Ricner Bellegarde",
    "Woodensky Pierre", "Dominique Simon",
    "Duckens Nazon", "Frantzdy Pierrot", "Derrick Etienne Jr.",
    "Louicius Deedson", "Ruben Providence", "Josue Casimir",
    "Yassin Fortune", "Wilson Isidor", "Lenny Joseph"),

  `Curaçao` = c(
    "Eloy Room", "Trevor Doornbusch", "Tyrick Bodak",
    "Jurien Gaari", "Roshon van Eijma", "Sherel Floranus", "Joshua Brenet",
    "Shurandy Sambo", "Armando Obispo",
    "Riechedly Bazoer", "Deveron Fonville", "Leandro Bacuna",
    "Juninho Bacuna", "Godfried Roemeratoe", "Kevin Felida",
    "Livano Comenencia", "Ar'jany Martha", "Tyrese Noslin",
    "Kenji Gorre", "Brandley Kuwas", "Gervane Kastaneer",
    "Jeremy Antonisse", "Jearl Margaritha", "Jurgen Locadia",
    "Sontje Hansen", "Tahith Chong"),

  `Cabo Verde` = c(
    "Vozinha", "Marcio Rosa", "CJ dos Santos",
    "Stopira", "Roberto Lopes", "Joao Paulo", "Diney", "Logan Costa",
    "Steven Moreira", "Wagner Pina", "Sidny Lopes Cabral", "Kelvin Pires",
    "Jamiro Monteiro", "Kevin Pina", "Deroy Duarte", "Telmo Arcanjo",
    "Laros Duarte", "Yannick Semedo",
    "Ryan Mendes", "Garry Rodrigues", "Willy Semedo", "Jovane Cabral",
    "Gilson Benchimol", "Dailon Livramento", "Helio Varela",
    "Nuno da Costa"),

  `Congo DR` = c(
    "Lionel Mpasi", "Timothy Fayulu", "Matthieu Epolo",
    "Chancel Mbemba", "Arthur Masuaku", "Gedeon Kalulu", "Joris Kayembe",
    "Dylan Batubinsika", "Axel Tuanzebe", "Aaron Wan-Bissaka",
    "Steve Kapuadi",
    "Samuel Moutoussamy", "Edo Kayembe", "Charles Pickel", "Gael Kakuta",
    "Noah Sadiki", "Aaron Tshibola", "Ngal'ayel Mukau", "Brian Cipenga",
    "Cedric Bakambu", "Meschak Elia", "Theo Bongonda", "Fiston Mayele",
    "Yoane Wissa", "Nathanael Mbuku", "Simon Banza")
)

# 2. Name-normalization helpers ----
# (Lifted from debug/wc2026_squad_crosscheck.R; the cross-check will eventually
# source this file so the two stay in sync.)

# Lowercase, strip accents, drop apostrophes/periods, collapse whitespace.
norm_name <- function(x) {
  x <- stringi::stri_trans_nfd(x)
  x <- gsub("[̀-ͯ]", "", x)        # remove combining marks
  x <- tolower(x)
  x <- gsub("[.'’`]", "", x)            # drop punctuation
  x <- gsub("\\s+", " ", trimws(x))
  x
}

# Candidate keys for fuzzy-match: full normalized, last token, last two tokens.
keys_for_name <- function(name) {
  n <- norm_name(name)
  toks <- strsplit(n, " ")[[1]]
  if (length(toks) == 0L) return(character(0))
  last <- toks[length(toks)]
  last2 <- if (length(toks) >= 2L) {
    paste(toks[(length(toks) - 1L):length(toks)], collapse = " ")
  } else {
    last
  }
  unique(c(n, last, last2))
}

# 3. Per-team resolver ----

#' Resolve one team's announced squad to Opta player_ids
#'
#' Looks each announced name up in `lineups` (already filtered to the team
#' and to international competitions). Falls back to surname / last-two
#' tokens when the full name doesn't match (handles diacritic / cultural
#' variants like "K. De Bruyne" vs "Kevin De Bruyne").
#'
#' Returns a data.table with one row per announced player; unresolved
#' players still appear but with NA player_id (the pipeline can log /
#' shrink them, same as any unrated player).
#'
#' @param team Opta `team_name`.
#' @param ann_names Character vector of announced player names (length 23-26).
#' @param lineups data.table of opta_lineups already filtered to the team
#'   and to international competitions (any other slicing is fine).
#' @param as_of `Date` for expected-minutes decay. Default Sys.Date().
resolve_team_announced_squad <- function(team, ann_names, lineups,
                                          as_of = Sys.Date()) {
  if (!data.table::is.data.table(lineups)) {
    lineups <- data.table::as.data.table(lineups)
  }
  if (nrow(lineups) == 0L) {
    warning(sprintf("[%s] no lineup rows — squad unresolvable", team), call. = FALSE)
    return(data.table::data.table(
      team_name = team, team_id = NA_character_,
      player_id = NA_character_, player_name = ann_names,
      position = NA_character_,
      expected_minutes_norm = 0,
      is_starter_pred = FALSE
    ))
  }

  # Build candidate (player_id -> set-of-keys) dict from Opta-side names.
  # When multiple player_ids share a key (e.g., a junior + senior namesake),
  # we'll prefer the one with the most recent appearance later.
  opta_unique <- unique(lineups[, .(player_id, player_name)])
  opta_keys <- lapply(opta_unique$player_name, keys_for_name)
  names(opta_keys) <- opta_unique$player_id

  # Last-appearance lookup for ambiguity tie-breaking
  last_seen <- lineups[, .(last_date = max(as.Date(sub("Z$", "", match_date)))),
                       by = player_id]

  resolved <- vector("list", length(ann_names))
  for (i in seq_along(ann_names)) {
    nm <- ann_names[i]
    ann_keys <- keys_for_name(nm)
    # Match: any overlap between announced keys and an opta player's keys
    hits <- vapply(opta_keys, function(k) length(intersect(k, ann_keys)) > 0L,
                   logical(1L))
    cand_ids <- names(opta_keys)[hits]
    pid <- if (length(cand_ids) == 0L) {
      NA_character_
    } else if (length(cand_ids) == 1L) {
      cand_ids
    } else {
      # Multiple hits — pick most-recently-seen
      tb <- last_seen[player_id %in% cand_ids]
      data.table::setorder(tb, -last_date)
      tb$player_id[1L]
    }
    resolved[[i]] <- data.table::data.table(
      announced_name = nm,
      player_id = pid
    )
  }
  out <- data.table::rbindlist(resolved)

  # Compute expected minutes for matched players; pull in modal position.
  matched_pids <- out[!is.na(player_id), player_id]
  if (length(matched_pids) >= 1L) {
    lu_matched <- lineups[player_id %in% matched_pids]
    em <- panna::build_team_expected_minutes(
      team = team,
      lineups = lu_matched,
      as_of = as_of,
      lookback_days = 1095L,
      squad_size = length(matched_pids)
    )
    if (nrow(em) > 0L) {
      em_dt <- data.table::as.data.table(em)
      out <- merge(out, em_dt[, .(player_id, player_name, position,
                                   expected_minutes_norm)],
                   by = "player_id", all.x = TRUE)
    }
  }
  # Fill in still-missing columns for unresolved/EM-less rows
  if (!"player_name" %in% names(out)) out[, player_name := NA_character_]
  if (!"position" %in% names(out)) out[, position := NA_character_]
  if (!"expected_minutes_norm" %in% names(out)) {
    out[, expected_minutes_norm := 0]
  }
  out[is.na(player_name), player_name := announced_name]
  out[is.na(expected_minutes_norm), expected_minutes_norm := 0]
  out[is.na(position), position := "Substitute"]

  # Pick the synthetic XI: top-1 GK + top-10 outfield by expected_minutes_norm.
  is_gk <- grepl("Goalkeeper", out$position, ignore.case = TRUE)
  out[, is_starter_pred := FALSE]
  data.table::setorder(out, -expected_minutes_norm)
  gk_idx <- which(is_gk)
  if (length(gk_idx) >= 1L) {
    out$is_starter_pred[gk_idx[1L]] <- TRUE
  }
  out_idx <- which(!is_gk)
  out$is_starter_pred[head(out_idx, 10L)] <- TRUE

  team_id <- lineups[!is.na(team_id), unique(team_id)]
  team_id_one <- if (length(team_id) == 0L) NA_character_ else team_id[1L]

  out[, team_name := team]
  out[, team_id := team_id_one]
  data.table::setcolorder(out, c("team_name", "team_id", "player_id",
                                  "player_name", "position",
                                  "expected_minutes_norm", "is_starter_pred",
                                  "announced_name"))
  out
}

# 4. Derived-squad resolver (non-announced WC2026 teams) ----
#
# For WC2026 teams that haven't yet published a 26-man squad, derive an
# EM-weighted 26-player squad from recent international appearances using
# `panna::build_team_expected_minutes()`. Same parquet schema as the
# announced rows, with `source = "derived"`, so step 02 / 02b can treat
# announced + derived squads identically — every WC2026 group-fixture
# team flows through minute-weighting, removing the "rotation tax"
# asymmetry whereby announced teams paid for substitutions but
# non-announced ones didn't.
#
# @param team Opta `team_name`.
# @param team_id_in Opta `team_id`.
# @param lineups Intl-filtered opta_lineups already filtered to the team.
# @param as_of Date for EM decay (default WC kickoff).
resolve_derived_squad <- function(team, team_id_in, lineups,
                                    as_of = as.Date("2026-06-11")) {
  if (!data.table::is.data.table(lineups)) {
    lineups <- data.table::as.data.table(lineups)
  }
  if (nrow(lineups) == 0L) {
    return(data.table::data.table(
      team_name = team,
      team_id   = team_id_in,
      player_id = character(0),
      player_name = character(0),
      position = character(0),
      expected_minutes_norm = numeric(0),
      is_starter_pred = logical(0),
      announced_name = character(0),
      source = character(0)
    ))
  }
  em <- panna::build_team_expected_minutes(
    team          = team,
    lineups       = lineups,
    as_of         = as_of,
    lookback_days = 1095L,
    squad_size    = 26L
  )
  if (nrow(em) == 0L) {
    return(data.table::data.table(
      team_name = team, team_id = team_id_in,
      player_id = character(0), player_name = character(0),
      position = character(0), expected_minutes_norm = numeric(0),
      is_starter_pred = logical(0), announced_name = character(0),
      source = character(0)
    ))
  }
  out <- data.table::as.data.table(em)
  # Mark top-1 GK + top-10 outfield as starter_pred (mirrors announced).
  is_gk <- grepl("Goalkeeper", out$position, ignore.case = TRUE)
  out[, is_starter_pred := FALSE]
  data.table::setorder(out, -expected_minutes_norm)
  gk_idx <- which(is_gk)
  if (length(gk_idx) >= 1L) out$is_starter_pred[gk_idx[1L]] <- TRUE
  out_idx <- which(!is_gk)
  out$is_starter_pred[head(out_idx, 10L)] <- TRUE

  out[, team_name := team]
  out[, team_id := team_id_in]
  out[, announced_name := player_name]
  out[, source := "derived"]
  out[, c("team_name", "team_id", "player_id", "player_name", "position",
          "expected_minutes_norm", "is_starter_pred", "announced_name",
          "source"), with = FALSE]
}

# 5. Driver ----

#' Build the wc2026_announced_squads parquet
#'
#' @param opta_lineups_path Path to opta_lineups.parquet (default points at
#'   the sister `pannadata` checkout, matching step 02's convention).
#' @param out_path Output parquet path.
#' @param squads Named list of announced squads. Defaults to the master
#'   list in this file.
#' @param as_of `Date` for the EM decay. Default `2026-06-11` (WC kickoff).
build_wc2026_announced_squads <- function(
    opta_lineups_path = "../pannadata/data/opta/opta_lineups.parquet",
    fixture_results_path = file.path("data-raw", "cache-predictions-opta",
                                       "01_fixture_results.rds"),
    out_path = file.path("data-raw", "cache-predictions-opta",
                         "wc2026_announced_squads.parquet"),
    squads = WC2026_ANNOUNCED_SQUADS,
    as_of = as.Date("2026-06-11"),
    include_derived = TRUE) {

  stopifnot(file.exists(opta_lineups_path))
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("'arrow' is required to read/write the announced-squads parquet.")
  }
  intl_comps <- c("AFCON", "AFCON_Qualifiers", "AFC_Asian_Cup",
                  "AFC_WC_Qualifiers", "Asian_Cup_Qualifiers",
                  "CAF_WC_Qualifiers", "CONCACAF_Gold_Cup", "Copa_America",
                  "Gulf_Cup_of_Nations", "Intl_Friendlies",
                  "UEFA_Euro_Qualifiers", "UEFA_Euros",
                  "UEFA_Nations_League", "UEFA_WC_Qualifiers", "World_Cup",
                  "CONMEBOL_WC_Qualifiers")

  lu_all <- data.table::as.data.table(arrow::read_parquet(opta_lineups_path))
  lu_intl <- lu_all[competition %in% intl_comps]
  rm(lu_all); invisible(gc(verbose = FALSE))

  out_parts <- vector("list", length(squads))
  names(out_parts) <- names(squads)
  for (team in names(squads)) {
    lu_t <- lu_intl[team_name == team]
    if (nrow(lu_t) == 0L) {
      message(sprintf("  [%s] WARN: 0 intl-competition lineup rows in opta_lineups",
                      team))
    }
    one <- resolve_team_announced_squad(
      team = team,
      ann_names = squads[[team]],
      lineups = lu_t,
      as_of = as_of
    )
    one[, source := "announced"]
    out_parts[[team]] <- one
    n_unres <- sum(is.na(one$player_id))
    if (n_unres > 0L) {
      message(sprintf("  [%s] %d / %d announced players unresolved",
                      team, n_unres, length(squads[[team]])))
    }
  }

  # Derive minute-weighted 26-man pools for WC2026 teams that haven't yet
  # announced — so the prediction pipeline applies the same weighting basis
  # to every WC2026 group-fixture team.
  derived_parts <- list()
  if (isTRUE(include_derived) && file.exists(fixture_results_path)) {
    fr <- data.table::as.data.table(readRDS(fixture_results_path))
    wc <- fr[league == "WC" &
              season == "2026 Canada-Mexico-USA" &
              match_status != "Played" &
              !is.na(home_team_id) & home_team_id != "" &
              !is.na(away_team_id) & away_team_id != ""]
    wc_teams <- unique(rbind(
      wc[, .(team_name = home_team, team_id = home_team_id)],
      wc[, .(team_name = away_team, team_id = away_team_id)]
    ))
    needs_derive <- wc_teams[!team_name %in% names(squads)]
    message(sprintf("Deriving WC2026 squads for %d teams without announced lists",
                    nrow(needs_derive)))
    for (i in seq_len(nrow(needs_derive))) {
      team <- needs_derive$team_name[i]
      team_id_in <- needs_derive$team_id[i]
      lu_t <- lu_intl[team_name == team]
      if (nrow(lu_t) == 0L) {
        message(sprintf("  [%s] WARN: derived squad — 0 intl lineup rows", team))
        next
      }
      d <- resolve_derived_squad(team, team_id_in, lu_t, as_of)
      if (nrow(d) > 0L) {
        derived_parts[[team]] <- d
        message(sprintf("  [%s] derived %d-player pool (top EM %.0f)",
                        team, nrow(d), max(d$expected_minutes_norm)))
      }
    }
  }

  out <- data.table::rbindlist(
    c(out_parts, derived_parts),
    fill = TRUE, use.names = TRUE
  )

  arrow::write_parquet(out, out_path)
  message(sprintf("Wrote %d rows: %d announced (%d teams) + %d derived (%d teams) -> %s",
                  nrow(out),
                  sum(out$source == "announced"), length(squads),
                  sum(out$source == "derived"), length(derived_parts),
                  out_path))
  invisible(out)
}

# 5. Run as script ----

if (sys.nframe() == 0L) {
  # Stand-alone: rebuild the parquet from the master list.
  if (!exists("WC2026_ANNOUNCED_SQUADS_SOURCE_ONLY") ||
      !isTRUE(WC2026_ANNOUNCED_SQUADS_SOURCE_ONLY)) {
    suppressPackageStartupMessages({
      library(data.table)
      devtools::load_all(".")
    })
    build_wc2026_announced_squads()
  }
}
