## 1 LOAD THE DATA
print("1. Loading the data")
load("data/input/UDLS/UDLS_tot.RData")

df1 = data.frame(UDLS_tot[1])
df2 = data.frame(UDLS_tot[2])
df3 = data.frame(UDLS_tot[3])
df4 = data.frame(UDLS_tot[4])
df5 = data.frame(UDLS_tot[5])
df6 = data.frame(UDLS_tot[6])

# Normalize the sets and append them together
#2,4 en 6 are structurally equal
#1 & 3 are structurally equal
#5 is similar to 1 & 3, but lacks "EINDTIJD"
#5 seems low quality all around
df2$GESCHATTE_EINDTIJD <- df4$GESCHATTE_EINDTIJD <- df6$GESCHATTE_EINDTIJD <- NULL
df1$EINDTIJD <- df2$EINDTIJD <- df3$EINDTIJD <- df4$EINDTIJD <- df6$EINDTIJD <- NULL
UDLS_tot = rbind(df1, df2, df3, df4, df5, df6)
rm(df1, df2, df3, df4, df5, df6)

# 2 CLEAN & SELECT
print("2. Cleaning the data, selecting relevant attributes")
UDLS_tot_clean = unique(UDLS_tot)

# Inhoudelijk minder interessant (handmatige inschatting)
clean_candidates = c("Pechgeval", "Spookrijder", "Shiftjournaal", "Regelscenario", "Overige", "Onwelwording", "Onbekend", "Hoogtemelding", "Handhaving", "Flitsprotocol", "Cross Border Management", "Controle OVM processen", "Cbm buitenland", "Achtergelaten Voertuig")
UDLS_tot_clean = UDLS_tot_clean[! UDLS_tot_clean$PROCES %in% clean_candidates,]
#We remove incidenttypes of lower frequencies
clean_candidates = aggregate(INCIDENTID ~ PROCES, UDLS_tot_clean, FUN = length)
clean_candidates = clean_candidates[clean_candidates[,2] < 10000,]
UDLS_tot_clean = UDLS_tot_clean[! UDLS_tot_clean$PROCES %in% clean_candidates$PROCES,]
#We remove the lanes of which we know we are not interested in (in this case we are only interested at he right most lanes)
clean_candidates = c("vr bb", "vr", "vl vr", "vl bb", "vl", "p vr", "p bb", "p","mb vr bb", "mb vr", "mb vl", "mb p", "mb bb", "mb", "bb")
UDLS_tot_clean = UDLS_tot_clean[! UDLS_tot_clean$RIJSTROKEN %in% clean_candidates,]
#We must have a WEGNAAM and a HECTORMETER VAN
UDLS_tot_clean = UDLS_tot_clean[complete.cases(UDLS_tot_clean[,21:22]),]
#We ignore all entries that are about more than one hectometer
UDLS_tot_clean = UDLS_tot_clean[UDLS_tot_clean$HECTOMETER_TOT == -1 ,]
rm(clean_candidates)

## 3. PROJECT THE DATA TO A CONVENIENT FORMAT
print("3. Projecting the data to target matrix")
UDLS_tot_clean_transformed = UDLS_tot_clean
#transforming date to period as used in GMS
UDLS_tot_clean_transformed$period <- as.numeric(format((as.Date(UDLS_tot_clean_transformed$STARTTIJD)),"%Y")) + as.numeric(format(as.Date(UDLS_tot_clean_transformed$STARTTIJD),"%m") > 10) # if(format(as.Date(UDLS_tot_clean_transformed$STARTTIJD),"%m") > 10){as.numeric(format((as.Date(UDLS_tot_clean_transformed$STARTTIJD)),"%Y")) + 1} else {as.numeric(format((as.Date(UDLS_tot_clean_transformed$STARTTIJD)),"%Y"))} 
#saving the location information
UDLS_tot_clean_transformed$weg <- UDLS_tot_clean_transformed$WEGNAAM
UDLS_tot_clean_transformed$hectometer <- UDLS_tot_clean_transformed$HECTOMETER_VAN
UDLS_tot_clean_transformed$richting  <-UDLS_tot_clean_transformed$RICHTING
#transforming proces into individual dimensions
UDLS_tot_clean_transformed$dieren <- as.numeric(UDLS_tot_clean_transformed$PROCES == "Dieren")
UDLS_tot_clean_transformed$ongeval <- as.numeric(UDLS_tot_clean_transformed$PROCES == "Ongeval")
UDLS_tot_clean_transformed$voorwerp <- as.numeric(UDLS_tot_clean_transformed$PROCES == "Voorwerp")
UDLS_tot_clean_transformed$voorwerp_op_rijstrook <- as.numeric(UDLS_tot_clean_transformed$PROCES == "Voorwerp op rijstrook")
UDLS_tot_clean_transformed$werk_in_uitvoering <- as.numeric(UDLS_tot_clean_transformed$PROCES == "Werk in uitvoering")
UDLS_tot_clean_transformed$infraschade = as.numeric(UDLS_tot_clean_transformed$SCHADE_AAN_INFRA =="true")
UDLS_tot_clean_transformed$schadelijke_stof = as.numeric(UDLS_tot_clean_transformed$SCHADELIJKE_STOFFEN =="true")
#removing all irrelevant dimensions
UDLS_tot_clean_transformed[,1:27] <- NULL
#aggregating to single records per period and location
UDLS_tot_clean_transformed = aggregate(cbind(dieren, ongeval, voorwerp, voorwerp_op_rijstrook, werk_in_uitvoering, infraschade, schadelijke_stof)~period+weg+hectometer+richting, data=UDLS_tot_clean_transformed, sum)

# 4. Clean and save the workspace
print("4. Cleaning the workspace, saving the results to file")
rm(UDLS_tot, UDLS_tot_clean)
save(UDLS_tot_clean_transformed, file="data/output/UDLS_tot_clean_transformed.Rdata")
