library(data.table)
combine_words = knitr::combine_words

# Classification of Economic Activities, Edition 2008 ----
# https://www.destatis.de/DE/Methoden/Klassifikationen/Gueter-Wirtschaftsklassifikationen/Downloads/klassifikation-wz-2008-englisch.html


classification = "Abschnitt A Land- und Forstwirtschaft, Fischerei
Abschnitt B Bergbau und Gewinnung von Steinen und Erden
Abschnitt C Verarbeitendes Gewerbe
Abschnitt D Energieversorgung
Abschnitt E Wasserversorgung; Abwasser- und Abfallentsorgung und Beseitigung von Umweltverschmutzungen
Abschnitt F Baugewerbe
Abschnitt G Handel; Instandhaltung und Reparatur von Kraftfahrzeugen
Abschnitt H Verkehr und Lagerei
Abschnitt I Gastgewerbe
Abschnitt J Information und Kommunikation
Abschnitt K Erbringung von Finanz- und Versicherungsdienstleistungen
Abschnitt L Grundstücks- und Wohnungswesen
Abschnitt M Erbringung von freiberuflichen, wissenschaftlichen und technischen Dienstleistungen
Abschnitt N Erbringung von sonstigen wirtschaftlichen Dienstleistungen
Abschnitt O Öffentliche Verwaltung, Verteidigung; Sozialversicherung
Abschnitt P Erziehung und Unterricht
Abschnitt Q Gesundheits- und Sozialwesen
Abschnitt R Kunst, Unterhaltung und Erholung
Abschnitt S Erbringung von sonstigen Dienstleistungen
Abschnitt T Private Haushalte mit Hauspersonal; Herstellung von Waren und Erbringung von Dienstleistungen durch Private Haushalte für den Eigenbedarf ohne ausgeprägten Schwerpunkt
Abschnitt U Exterritoriale Organisationen und Körperschaften"


classification = strcapture(
  "(?:Abschnitt ([A-U])) (.*)",
  strsplit(classification, "\n")[[1]],
  data.frame(code = character(21), sector_de = character(21))
)



# klassifikation wz2008 English
klassifikation <- readxl::read_excel("data/raw/Regional-Atlas/klassifikation-wz-2008-englisch.xls",
  sheet = "WZ 2008", skip = 1L,
  col_names = c("lfd_nr", "code", "sector_en", "isic")
)

klassifikation = subset(klassifikation[, c(-1, -4)], grepl("^[A-Z]$", code))



classification = merge(classification, klassifikation, by = "code")
classification$sector_en = tools::toTitleCase(tolower(classification$sector_en))
rm(klassifikation)


setDT(classification)

class_sub = dcast(classification[, !"sector_de"], . ~ code, value.var = "sector_en")

class_sub = with(
  class_sub,
  c(
    paste0(A, " (A)"),
    combine_words(sprintf("%s (%s)", c(B, D, E), c("B", "D", "E"))),
    paste0(`C`, " (C)"),
    paste0(`F`, " (F)"),
    combine_words(sprintf("%s (%s)", c(G, H, I, J), c("G", "H", "I", "J"))),
    combine_words(sprintf("%s (%s)", c(K, L), c("K", "L"))),
    combine_words(sprintf("%s (%s)", c(O, P, Q, R, S, `T`), c("O", "P", "Q", "R", "S", "T")))
  )
)


class_sub = c(
  "Agriculture, Forestry and Fishing (A)",
  "Mining and Quarrying (B), Electricity, Gas, Steam and Air Conditioning Supply (D), and Water Supply; Sewerage, Waste Management and Remediation Activities (E)",
  "Manufacturing (C)",
  "Construction (F)",
  "Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles (G), Transportation and Storage (H), Accommodation and Food Service Activities (I), and Information and Communication (J)",
  "Financial and Insurance Activities (K) and Real Estate Activities (L)",
  "Public Administration and Defence; Compulsory Social Security (O), Education (P), Human Health and Social Work Activities (Q), Arts, Entertainment and Recreation (R), Other Service Activities (S), and Activities of Households as Employers; U0ndifferentiated Goods- And Services-Producing Activities of Households for Own Use (T)"
)


class_sub_short = c(
  "Agri, forestry, and fishing",
  "Mining & quarrying, energy, and water, sewage & waste",
  "Manufacturing",
  "Construction",
  "Trade, transport, hospitality, and info & communication",
  "Finance & insurance, and real estate",
  "Public & other services, educ, and health"
)



## industry classification in the data
##  according to Wirtschaftszweigklassifikation WZ 2008 ----
industry_classes = data.frame(
  code = c("WZ08-A", "WZ08-B-18", "WZ08-C", "WZ08-F", "WZ08-G-01-I-J", "WZ08-KL", "WZ08-PX"),
  label_en = c(
    "Agriculture, forestry, fishing (A)",
    "Manufacturing industry excluding construction (B,D,E)",
    "Manufacturing Industry (C)",
    "Construction Industry (F)",
    "Trade, transport, hospitality, information / communication (G-J)",
    "Financial, insurance and corporate service providers, Real estate and housing (K,L)",
    "Public and other services, education, health (O-T)"
  ),
  label_de = c(
    "Land- und Forstwirtschaft, Fischerei (A)",
    "Produzierendes Gewerbe ohne Baugewerbe (B,D,E)",
    "Verarbeitendes Gewerbe (C)",
    "Baugewerbe (F)",
    "Handel, Verkehr, Gastgewerbe, Information und Kommunikation (G-J)",
    "Finanz-, Versicherungs- und Unternehmensdienstleister, Grundstücks- und Wohnungswesen (K,L)",
    "Öffentliche und sonstige Dienstleister, Erziehung, Gesundheit (O-T)"
  )
)
