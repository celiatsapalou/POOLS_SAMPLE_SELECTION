library("readxl")
library(stringr)
library(openxlsx)
library(dplyr)

#load ONT DATA
ONT_data <- read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/1k_ont_data_overview.xlsx")

#LOAD data for pools 1,2,3
compare<-read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/compare.xlsx")

# remove 1,2,3, ordered pools
ONT_after_pools <- ONT_data[!(ONT_data$SAMPLE %in% compare$`Pool1/2/3`), ]

#load Eichler's data
Eichler_final <- read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/Eichler_2107_LAST.xlsx")

# Check for duplicates in Eichler's dataset and remove them (i noticed there were some duplicates)
#Eichler_no_duplicates <- distinct(Eichler_final, SAMPLE, .keep_all = TRUE)
#write.xlsx(Eichler_no_duplicates, "/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/EICHLER_FINAL_ALL_2007.xlsx")

# Use anti_join to remove rows in ONT dataset that are included in Eichler's
ONT_filtered <- anti_join(ONT_after_pools, Eichler_final, by = "SAMPLE")
write.xlsx(ONT_filtered, "/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/1k_ONT_2107.xlsx")

#check and keep the common samples for possible future usage
removed_samples <- semi_join(ONT_after_pools, Eichler_data, by = "SAMPLE")

##########
## I then added Eichler's data to the ONT dataset MANUALLY ON EXCEL, to have a complete dataset ############
##########

#LOAD THE FINAL DATASET
all_data <- read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/1k_ONT_2107.xlsx")
all_data <- read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/FINALIZED_ALL_SAMPLES_2107.xlsx")

coverages_file = '/Users/tsapalou/Downloads/PhD-2023/Coverage_Pools/coverages_processed/coverages_all.tsv'
ancestries_file = '/Users/tsapalou/Downloads/PhD-2023/Coverage_Pools/populations/1kgp_samples_pops_threecol.tsv'

# Load stuff
covs = read.table(coverages_file, sep='\t', header=T)
ancs = read.table(ancestries_file, sep='\t', header=F)
colnames(ancs) = c('SAMPLE', 'pop','superpop')

# Combine the two dfs
#covs_with_ancs = left_join(covs, ancs, by='SAMPLE')

# Combine the two dfs
covs_with_ancs = left_join(all_data, ancs, by='SAMPLE')

### keep 20 samples with highest coverage per population 
all_data_pop<- covs_with_ancs  %>%
  group_by(ANCESTRY) %>%
  arrange(desc(T2T_median_cov)) %>%
  slice(if(n() >= 20) 1:20 else 1:n())


#### JUST REPLACING NAS in superpop column 
selected_cols <- selected_cols %>%
  mutate(pop.y = ifelse(SAMPLE == 'NA15730' & is.na(pop.y), 'CEU', pop.y),
         superpop.y = ifelse(SAMPLE == 'NA15730' & is.na(superpop.y), 'EUR', superpop.y),
         pop.col = ifelse(SAMPLE == 'NA15730' & is.na(pop.col), '#2356A7', pop.col),
         superpop.col = ifelse(SAMPLE == 'NA15730' & is.na(superpop.col), '#018ead', superpop.col))

selected_cols <- selected_cols %>%
  mutate(
    pop.y = ifelse(SAMPLE == 'HG18593' & is.na(pop.y), 'CHS', pop.y),
    superpop.y = ifelse(SAMPLE == 'HG18593' & is.na(superpop.y), 'EAS', superpop.y),
    pop.col = ifelse(SAMPLE == 'HG18593' & is.na(pop.col), '#64BC46', pop.col),
    superpop.col = ifelse(SAMPLE == 'HG18593' & is.na(superpop.col), '#778500', superpop.col)
  )

selected_cols <- selected_cols %>%
  mutate(
    pop.y = ifelse(SAMPLE == 'NA20820' & is.na(pop.y), 'TSI', pop.y),
    superpop.y = ifelse(SAMPLE == 'NA20820' & is.na(superpop.y), 'EUR', superpop.y),
    pop.col = ifelse(SAMPLE == 'NA20820' & is.na(pop.col), '#263877', pop.col),
    superpop.col = ifelse(SAMPLE == 'NA20820' & is.na(superpop.col), '#018ead', superpop.col)
  )



# Calculate the frequency of each population
pop_freq <-selected_cols %>%
  group_by(pop.y, pop.col) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency))  # Arrange by descending frequency


# Create the bar plot
ggplot(pop_freq, aes(x = reorder(pop.y, -frequency), y = frequency, fill = pop.col)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # This tells ggplot to use the actual color values from pop.col
  theme_minimal() +
  labs(x = "Population", y = "Frequency", fill = "Population Color") +
  theme(legend.position = "none",  # Hide the legend since colors are explicit
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability

ggplot(pop_freq, aes(y = reorder(pop.y, -frequency), x = frequency, fill = pop.col)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() + 
  coord_flip() +  # Flip the coordinates to make the bars horizontal
  theme_minimal() +
  labs(y = "Population", x = "Frequency", fill = "Population Color") +
  theme(legend.position = "none")

# Calculate the frequency of each population
superpop_freq <-selected_cols %>%
  group_by(superpop.y, superpop.col) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(desc(frequency))  # Arrange by descending frequency


# Create the bar plot
ggplot(superpop_freq, aes(x = reorder(superpop.y, -frequency), y = frequency, fill = superpop.col)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # This tells ggplot to use the actual color values from pop.col
  theme_minimal() +
  labs(x = "Superpopulation", y = "Frequency", fill = "Population Color") +
  theme(legend.position = "none",  # Hide the legend since colors are explicit
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better readability



pool.df %>% group_by(ID, superpop) %>% summarise(count=n()) %>% arrange(desc(count)) %>% mutate(superpop = factor(superpop, levels = unique(superpop))) %>%
  ggplot() +
  geom_col(aes(x=superpop, y=count, fill=superpop)) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels=comma) +
  xlab('') +
  ylab('Unique samples per superpop') +
  theme_minimal()


all_data_pop <- all_data_pop %>%
  mutate(superpop = ifelse(SAMPLE == 'NA20820' & is.na(superpop), 'EUR', superpop))

all_data_pop <- all_data_pop %>%
  mutate(superpop = ifelse(SAMPLE == 'HG10593' & is.na(superpop), 'EAS', superpop))

all_data_pop <- all_data_pop %>%
  mutate(superpop = ifelse(SAMPLE == 'HG18593' & is.na(superpop), 'EAS', superpop))

## Load 1000G colors
sample.col <- read.table("/Users/tsapalou/Downloads/U24_Subpopulations_Colors.csv", sep = ',', header=TRUE, stringsAsFactors = FALSE, comment.char = '&')
all_data_pop$pop.col <- sample.col$pop_color[match(all_data_pop$pop.y, sample.col$Population)]
all_data_pop$superpop.col <- sample.col$Spop_color[match(all_data_pop$superpop.y, sample.col$SuperPopulation)]

COV_TO_INCLUDE = 15

ggplot(all_data_pop) + 
  #geom_boxplot(aes(x=ANCESTRY, y=T2T_median_cov), fill='grey') +
  geom_beeswarm(aes(x=ANCESTRY, y=T2T_median_cov, color=superpop), cex=0.3) +
  geom_hline(aes(yintercept=COV_TO_INCLUDE)) + 
  ylim(c(0,60)) +
  theme_bw() + labs(y='Median Coverage')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(size = 5)) 

frequency_table <- as.data.frame(table(all_data_pop$ANCESTRY))

# Create a summary table for max and min coverage
summary_table <- all_data_pop %>%
  group_by(ANCESTRY) %>%
  summarize(
    frequency = n(),
    min_T2T_median_cov = min(T2T_median_cov, na.rm = TRUE),
    max_T2T_median_cov = max(T2T_median_cov, na.rm = TRUE)
  )

write.xlsx(summary_table, "/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/summary_table.xlsx")


# Identify samples that are not in the already selected samples
samples_remaining <- anti_join(all_data, all_data_pop, by = "SAMPLE")

# Count the number of rows in samples_remaining
nrows <- nrow(samples_remaining)

# Number of samples to select, in my case 233 remaining
n_samples <- min(233, nrows)

# Select the top n_samples based on T2T_median_cov
cov_additional <- samples_remaining %>%
  arrange(desc(T2T_median_cov)) %>%
  slice_head(n = n_samples)

# Combine the two dfs
cov_additional_anc = left_join(cov_additional, ancs, by='SAMPLE')

# Combine all_data_pop and cov_additional
combined <- bind_rows(all_data_pop, cov_additional_anc)


ggplot(combined) + 
  #geom_boxplot(aes(x=superpop, y=T2T_median_cov), fill='grey') +
  geom_beeswarm(aes(x=ANCESTRY, y=T2T_median_cov, color=superpop), cex=0.3) +
  geom_hline(aes(yintercept=COV_TO_INCLUDE)) + 
  ylim(c(0,70)) +
  theme_bw() + labs(y='Median Coverage')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(size = 5)) 

library(dplyr)
selected_cols <- all_data_pop %>% select(SAMPLE, ANCESTRY, pop.y, superpop.y, pop.col, superpop.col)


frequency<- as.data.frame(table(combined$superpop))

# Sort the frequency_table by Freq in descending order
frequency<- frequency[order(-frequency$Freq),]


# Calculate the frequency of each population
pop_freq <- selected_cols %>%
  group_by(pop.y, pop.col) %>%
  summarise(frequency = n()) %>%
  ungroup()  # Ungroup for plotting

# Create the plot
ggplot(pop_freq, aes(x = pop.y, y = frequency, fill = pop.col)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +  # Use the actual colors specified in pop.col
  theme_minimal() +
  labs(x = "Population", y = "Frequency", fill = "Population Color") +
  theme(legend.position = "bottom")  # Adjust legend position as needed



# Create the ggplot with reorder() function
ggplot(frequency, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Superpopulation Frequencies", x = "Superopulation", y = "Frequency") +
  theme_minimal()


ggplot(frequency_table, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Population Frequencies", x = "Population", y = "Frequency") +
  theme_minimal()

write.xlsx(combined, "/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/FINALIZED_2107.xlsx")


# Create a summary table for max and min coverage
summary_table <- combined %>%
  group_by(ANCESTRY) %>%
  summarize(
    frequency = n(),
    min_T2T_median_cov = min(T2T_median_cov, na.rm = TRUE),
    max_T2T_median_cov = max(T2T_median_cov, na.rm = TRUE)
  )

write.xlsx(summary_table, "/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/summary_table.xlsx")

#LOAD data for pools 1,2,3
finalized<-read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/FINALIZED_2107.xlsx")
ont_test<-read_excel("/Users/tsapalou/Downloads/PhD-2023/POOLS_2023/1k_ONT_2107.xlsx")

specific_value <- "Han Chinese South [CHS]"

file1_filtered <- finalized %>%
  filter(ANCESTRY == specific_value)

file2_filtered <- ont_test %>%
  filter(ANCESTRY == specific_value)

difference <- anti_join(file2_filtered, file1_filtered, by = "SAMPLE")
