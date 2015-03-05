library(dplyr)
library(ggplot2)
library(gridExtra)


# needed enough colours to be able to distinguish each of the 40 states in the scatterplots
# found this at www.r-bloggers.com/discrete-colors-in-ggplot/
gs.pal <- colorRampPalette(c("red", "blue", "orange", "yellow"), bias=1, space = "rgb")

# read in population data downloaded from census.gov
pop <- read.csv('pop_by_state.csv')
pop <- tbl_df(pop)

# caluclate ratio of 65+ population to total population (by state and available year)
# the values for the population are estimates based on the 2010 Census data
pop <- mutate(pop, YR2010_percent = (YR2010_65_plus / YR2010_Total_pop) * 100,
              YR2011_percent = (YR2011_65_plus / YR2011_Total_pop) * 100,
              YR2012_percent = (YR2012_65_plus / YR2012_Total_pop) * 100)

# Only interested in the 2012 data as that is the year of the Medicare payment data
pop_2012 <- select(pop, ID, YR2012_65_plus, YR2012_percent)
# rename 'state' column name to match the column in the Medicare data
colnames(pop_2012)[1] <- "nppes_provider_state" 

# read in drug use/abuse data downloaded from samhsa.gov
drug_use <- read.csv('drug_abuse.csv')
drug_use <- tbl_df(drug_use)

# ok, this section (until the '*************' below) still needs  work
# issues to fix: 
# 1. either be able to point to the URL and grab all the data files or point to a directory 
# 2. be able to read in the xlxs files directly
# 3. deal with the fact that the payment columns have $ signs that R does not seem to like
# 4. get R to recognize the integers as integers and the floats as floats
# 5. be able to process one file at a time to avoid memory issues
#
# basic set up is to read in a Medicare file (organized by last name of service provider)
# convert the # of claims and unique beneficiary columns to integer class
# convert zip code to factor (instead of integer) -- didn't use in this exploration but would be useful
# filter to only consider procedures at place_of_service being 'Office' -- removes any 'Facility' payments like hospitals, etc.
# filter to only consider 50 US states + DC (removes army installments and other US territories)
# select only the necessary columns -- more from a memory need than anything else
# bind the current file to the total Medicare dataframe
#phys_A <- read.csv("physician_2012_A.csv")
#phys_B <- read.csv("physician_2012_B.csv")
#phys_CD <- read.csv("physician_2012_CD.csv")
#phys_CD[,19:21] <- sapply(phys_CD[,19:21], as.integer)
#phys_CD[,11] <- sapply(phys_CD[,11], as.factor)
#phys_EFG <- read.csv("physician_2012_EFG.csv")
#phys_EFG[,19:21] <- sapply(phys_EFG[,19:21], as.integer)
#phys_HIJ <- read.csv("physician_2012_HIJ.csv")
#phys_HIJ[,19:21] <- sapply(phys_HIJ[,19:21], as.integer)
#phys_KL <- read.csv("physician_2012_KL.csv")
#phys_KL[,19:21] <- sapply(phys_KL[,19:21], as.integer)
#phys_MN <- read.csv("physician_2012_MN.csv")
#phys_MN[,19:21] <- sapply(phys_MN[,19:21], as.integer)
#phys_OPQ <- read.csv("physician_2012_OPQ.csv")
#phys_OPQ[,19:21] <- sapply(phys_OPQ[,19:21], as.integer)

phys_A <- read.csv("physician_2012_A.csv")
phys_A <- tbl_df(phys_A)

irregular_states <- c('XX', 'AA', 'AE', 'AP', 'AS', 'GU', 'MP', 'PR', 'VI', 'ZZ')
phys_A <- filter(phys_A, place_of_service == 'O', nppes_provider_country == 'US', !(nppes_provider_state %in% irregular_states)) 

phys_A <- select(phys_A, npi, nppes_provider_last_org_name, nppes_provider_first_name,
                 nppes_credentials, nppes_entity_code, nppes_provider_state,
                 provider_type, hcpcs_code, hcpcs_description,
                 line_srvc_cnt, bene_unique_cnt,
                 average_Medicare_payment_amt, stdev_Medicare_payment_amt)

phys_A[,10:11] <- sapply(phys_A[,10:11], as.integer)
phys_A[,1] <- sapply(phys_A[,1], as.factor)

phys_tbl <- phys_A

#phys_tbl <- rbind(phys_tbl, phys_YZ)

#phys <- do.call(rbind, list(phys_A, phys_B, phys_CD, phys_EFG, phys_HIJ, 
#                            phys_KL, phys_MN, phys_OPQ, phys_R))
# 
# ***************************************************************

# calculate the medicare cost, claims per uniques beneficiary and variance per line
phys_tbl <- mutate(phys_tbl, 
                   Medicare_cost = line_srvc_cnt * average_Medicare_payment_amt,
                   claims_per_beneficiary = line_srvc_cnt / bene_unique_cnt,
                   variance = (stdev_Medicare_payment_amt)^2)

# create dataframe of unique medical procedures contained in the database using hcpcs_description column
tests <- select(phys_tbl, hcpcs_description)
unique_tests <- unique(tests)

z_score_test <- numeric()

for (test in unique_tests$hcpcs_description) {
  print(test)
  # mechanism to randomly sample n unique tests and store as a vector
#  new_sample <- unique_tests[sample(nrow(unique_tests), 1), ]
#  new_sample <- as.vector(new_sample$hcpcs_description)
  new_sample <- as.vector(test)
  
  # get total Medicare payments by state
  total_by_state <- phys_tbl %>%
                      group_by(nppes_provider_state) %>%
                      summarize(
                                Medicare_total = sum(Medicare_cost, na.rm=TRUE),
                                stdev_total = sqrt(sum(variance, na.rm=TRUE)),
                                total_claims = mean(claims_per_beneficiary, na.rm=TRUE)
                                ) %>%
                      arrange(desc(Medicare_total))
  
  # define vector of tests of interest: new_sample gives random sample, hard coded version for just drug tests
  drug_tests <- new_sample
  #drug_tests <- c('Assay of opiates', 'Assay of cocaine', 'Assay of barbituates', 'Assay of amphetamines')
  #                'Assay of methadone', 'Assay of dihydromorphinone',
  #                'Assay of dihydrocodeinone', 
  #                'Assay of benzodiazepines')
  
  # define total payments by state for office visits 
  office_tests <- c('Office/outpatient visit new', 'Office/outpatient visit est')
  office_state <- phys_tbl %>%
                  #                    group_by(nppes_provider_state=='NC') %>% 
                  filter(provider_type=='Family Practice', hcpcs_description %in% office_tests) %>%
                  group_by(nppes_provider_state) %>%
                  summarize(
                    Med_total = sum(Medicare_cost , na.rm=TRUE)
  #                  drug_claims = mean(claims_per_beneficiary, na.rm=TRUE)
                  ) %>%
                  arrange(desc(Med_total))
  
  # get drug test payment totals by state
  drugs_by_state <- phys_tbl %>%
                      group_by(nppes_provider_state) %>% 
                      filter(hcpcs_description %in% drug_tests) %>%
                      summarize(
                        Medicare_drug_total = sum(Medicare_cost , na.rm=TRUE),
                        stdev_drugs = sqrt(sum(variance, na.rm=TRUE)),
                        drug_claims = mean(claims_per_beneficiary, na.rm=TRUE)
                      ) %>%
                      arrange(desc(Medicare_drug_total))
  
  # get drug payment totals by provider for North Carolina
#  nc_state <- phys_tbl %>%
#  #                    group_by(nppes_provider_state=='NC') %>% 
#                      filter(nppes_provider_state=='NC', hcpcs_description %in% drug_tests) %>%
#                      group_by(npi) %>%
#                      summarize(
#                        nppes_provider_last_org_name = nppes_provider_last_org_name,
#                        Medicare_drug_total = sum(Medicare_cost , na.rm=TRUE),
#                        drug_claims = mean(claims_per_beneficiary, na.rm=TRUE)
#                      ) %>%
#                      arrange(desc(Medicare_drug_total))
#  
#  # get the names of the providers for North Carolina (above only gets provider ID) and select needed columns
#  nc_name_get <- filter(phys_tbl, nppes_provider_state=='NC', hcpcs_description %in% drug_tests)
#  nc_merge <- merge(nc_name_get, nc_state, by='npi')
#  nc_merge <- select(nc_merge, npi, nppes_provider_last_org_name, provider_type, line_srvc_cnt)
  
  # join the Medical payment total data with the drug payment total and align by state
  # calculate ratio of total to drug by state and the standard error
  merged_drugs_by_state <- merge(total_by_state, drugs_by_state, by='nppes_provider_state')
  merged_drugs_by_state <- mutate(merged_drugs_by_state, 
                                  Medicare_percent = (Medicare_drug_total / Medicare_total) * 100,
                                  se = Medicare_percent * sqrt( (stdev_drugs / Medicare_drug_total)^2 + (stdev_total / Medicare_total)^2)) 
  
  # join above dataframe with population dataframe and align by state and do the same with drug use/abuse dataframe
  merged_drugs_by_state_pop <- merge(merged_drugs_by_state, pop_2012, by='nppes_provider_state')
  merged_drug_use_by_state <- merge(merged_drugs_by_state, drug_use, by='nppes_provider_state')
  
  # create by state dataframe for only drug payment percent in order to calculate the mean and standard deviation and z score by state
  # for a set of tests -- here done for the drug tests but used also for a number of randomly selected tests
  med_percent <- select(merged_drugs_by_state, nppes_provider_state, Medicare_percent)
  percent <- med_percent$Medicare_percent
  mean_mp <- mean(percent)
  stdev_mp <- sd(percent)
  med_percent <-mutate(med_percent, z_score = (Medicare_percent - mean_mp) / stdev_mp)
  new_scores <- as.vector(med_percent$z_score)
  z_score_test <- c(z_score_test, new_scores)
}
# plot total Medicare claims against claims for drug tests by state
ggplot(data=merged_drug_use_by_state, aes(x=total_claims, y=drug_claims, color=nppes_provider_state)) +
  geom_point() +
  scale_color_hue(l=10)

# for a set of tests plot state against z score, where z score is calculated based on mean of Medicare percent
ggplot(data=med_percent, aes(x=nppes_provider_state, y=z_score)) +
  geom_bar(stat='identity') 

#merged_drugs_by_state_ordered <- arrange(merged_drugs_by_state, desc(Medicare_percent))

# plot of state against percent of Medicare payments for drug tests
p2 <- ggplot(data=merged_drugs_by_state, aes(x=nppes_provider_state, y=Medicare_percent, fill = 'blue')) +
            geom_bar(color='black', stat='identity') +  
            xlab("State") + ylab("Percent of Medicare Payments for Drug Tests") + 
            theme(legend.position="none") + 
            ggtitle("Drug Test Payments by State") +
            theme(plot.title = element_text(lineheight = .8, face = 'bold'))

#ggplot(merged_drugs_by_state_pop, aes(x=Medicare_total/1000000, y= Medicare_drug_total/1000000, size=YR2012_65_plus, color=nppes_provider_state)) +
#        geom_point(shape=1) +
#        scale_color_hue(l=30)

# plot of 65+ population against total Medicare payments by state
p3 <- ggplot(merged_drugs_by_state_pop, aes(x=YR2012_65_plus/1000, y= Medicare_total/1000000, color=nppes_provider_state)) +
              geom_point(size=4) +
#              guides(col=guide_legend(nrow=10)) + 
              scale_color_manual(values=gs.pal(40)) +
              theme(legend.position="none") +
              ylab("Total Medicare Payments (in millions)") +
              xlab("Population 65 and older (in thousands)") +
              ggtitle("Total Payments vs Medicare Population") +
              theme(plot.title = element_text(lineheight = .8, face = 'bold'))

# plot of 65+ population against total Medicare payments for drug tests 
p4 <- ggplot(merged_drugs_by_state_pop, aes(x=YR2012_65_plus/1000, y= Medicare_drug_total/1000000, color=nppes_provider_state)) +
              geom_point(size=4) +
#              scale_color_manual(values=gs.pal(40)) +
              scale_color_manual(name = "       STATE", values=gs.pal(40)) +
              guides(col=guide_legend(nrow=20)) +
#              theme(legend.position="none") +
              ylab("Medicare Payments for Drug Tests (in millions)") +
              xlab("Population 65 and older (in thousands)") +
              ggtitle("Drug Tests Payments vs Medicare Population") +
              theme(plot.title = element_text(lineheight = .8, face = 'bold'))


legend34 <- ggplot(merged_drugs_by_state_pop, aes(x=YR2012_65_plus, y= Medicare_total/1000000, color=nppes_provider_state)) +
              geom_point(size=4)  +
              scale_color_manual(name = "               STATE", values=gs.pal(40)) +
              guides(col=guide_legend(nrow=14))

get_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


leg <- get_legend(legend34)
p34 <- grid.arrange(arrangeGrob(p3,p4, leg, ncol=3, widths= c(3/7, 3/7, 1/7)))

# plot of percentage of 18+ population using drugs against percent of Medicare payments for drug tests
p5 <- ggplot(data=merged_drug_use_by_state, aes(x=drug_use, y=Medicare_percent, color=nppes_provider_state)) +
            geom_point(size=4) +
            guides(col=guide_legend(nrow=20)) +
            scale_color_manual(name = "       STATE", values=gs.pal(40)) +
            ylab("Percent of Medicare Payments for Drug Tests") +
            xlab("Prevalence of Illicit Drug Use Age 18 and older (percent)") +
            ggtitle("Drug Tests Payments vs Drug Use in Broader Population") +
            theme(plot.title = element_text(lineheight = .8, face = 'bold')) +
            xlim(0,6)
#            theme(legend.title = element_text(color="black")) +
#            scale_color_discrete(name="State")

#}
#  theme( axis.text.x  = element_text(angle=90)) 


# multiple plot function from the R Cookbook
#
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}










