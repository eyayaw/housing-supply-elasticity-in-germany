# Missing Values and Variables to Exclude ---------------------------------
# missing values in the data sets are encoded as follows
missing_vals = "-11 anonymisiert
-10 Variable ist erst in zukünftigen Eingabemasken vorhanden
-9 Sonstiges Missing
-8 Variable trifft auf diesen Datensatzyp nicht zu
-7 keine Angabe
-6 Variable nicht mehr in Einagbemaske vorhanden
-5 unplausibler Wert geloescht"

# translation
## "-11 anonymized
## -10 Variable is only available in future input masks
## -9 Other Missing
## -8 Variable does not apply to this data record type
## -7 not specified
## -6 Variable no longer available in the input mask
## -5 implausible value deleted"

missing_vals = strcapture(
  "(-\\d{1,2})\\s(.*)",
  strsplit(missing_vals, "\n")[[1]],
  data.frame(value = integer(), label = character())
)

# variables that are not needed for our analysis?
exclude <- c(
  # does not apply to any data set
  "nebenraeume","betreut",
  # doesn't apply to this (house-buy) data set
  "kategorie_Wohnung", "nebenkosten", "balkon", "garten", "einbaukueche",
  "mietewarm", "mietekalt", "mietekaution",
  # few obs of the categories they include
  "aufzug", "heizkosten", "rollstuhlgerecht",
  # ---
  "ajahr", "amonat", "wohngeld", "hits", "immobilientyp",
  "einliegerwohnung", "emonat", "energieausweistyp", "energieeffizienzklasse",
  paste0("click_", c("schnellkontakte", "customer", "url", "weitersagen")),
  "duplicateid", # contains only -9
  "ev_wwenthalten", "etage",
  "foerderung", "haustier_erlaubt", "heizkosten_in_wm_enthalten",
  "kaufvermietet", "laufzeittage", "lieferung",
  "freiab", "courtage", "r1_id", paste0("bef", 1:10)
)

# variables that may include missing values (for both houses and apartments)
include_missings = c("obid", "nebenkosten", "kaufpreis", "mieteinnahmenpromonat", "heizkosten", "baujahr", "letzte_modernisierung", "wohnflaeche", "grundstuecksflaeche", "nutzflaeche", "etage", "anzahletagen", "zimmeranzahl", "nebenraeume", "schlafzimmer", "badezimmer", "parkplatzpreis", "wohngeld", "ev_kennwert", "laufzeittage", "hits", "click_schnellkontakte", "click_customer", "click_weitersagen", "click_url", "immobilientyp", "ajahr", "amonat", "ejahr", "emonat", "aufzug", "balkon", "betreut", "denkmalobjekt", "einbaukueche", "einliegerwohnung", "ev_wwenthalten", "ferienhaus", "foerderung", "gaestewc", "garten", "heizkosten_in_wm_enthalten", "kaufvermietet", "keller", "parkplatz", "rollstuhlgerecht", "bauphase", "ausstattung", "energieeffizienzklasse", "energieausweistyp", "haustier_erlaubt", "heizungsart", "kategorie_Wohnung", "kategorie_Haus", "objektzustandobid", "objektzustand")
