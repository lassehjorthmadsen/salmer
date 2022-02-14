library(tidyverse)
library(devtools)
library(DT)
library(salmer)
# load_all()

# Get pronunciation dictionary
pronunciation <- readRDS("data-raw/pronunciation.rds")

test_hymn <- 15

# De ti mest afspillede salmer på denne side:
# 754 Se, nu stiger solen af havets skød
# 448 Fyldt af glæde over livets under
# 522 Nåden er din dagligdag
# 192 Hil dig, Frelser og Forsoner
# 787 Du, som har tændt millioner af stjerner
# 582 At tro er at komme
# 749 I østen stiger solen op
# 15 Op, al den ting, som Gud har gjort
# 731 Nu står der skum fra bølgetop
# 729 Nu falmer skoven trindt om land

# cut up
cu <- cut_up(ref_id = test_hymn, df = annotated_hymns, except = "PUNCT")
cu_readable <- cu %>% collapse_annotation(token_new) %>% select(`Cut-up` = text)

# new rhymes
nr <- cu %>% new_rhymes(annotated_hymns)
nr_readable <- nr %>% collapse_annotation(token_new) %>% select(`Fixed rhymes` = text)

# Rhyme scheme
rs <- cu %>%
  filter(upos != "PUNCT") %>%
  group_by(line_id) %>%
  slice_max(token_id) %>%
  ungroup() %>%
  select(`Rhyme scheme` = rhyme_scheme)

# show final cut-up version along with original and other bits
comparison <- hymns %>%
  filter(doc_id == test_hymn) %>%
  select(Verse = verse, Original = text) %>%
  bind_cols(cu_readable, nr_readable, rs)

options(DT.options = list(dom = 't', pageLength = 100, autoWidth = TRUE))

datatable(comparison, rownames = F,
            options = list(columnDefs = list(list(width = '10px', targets = c(0, 4)))))


# Test some (possible) rhyms:
get_rhymes("borg", pronunciation)
get_rhymes("kartofler", pronunciation)
get_rhymes("smerte", pronunciation)
get_rhymes("svin", pronunciation)
get_rhymes("fisk", pronunciation)

# Find a great first verse
my_hymn <- 15
no_tries <- 200

verses <- annotated_hymns %>%
  filter(doc_id == my_hymn) %>%
  pull(verse) %>%

rs <- annotated_hymns %>%
  filter(doc_id == my_hymn, verse == 1, upos != "PUNCT") %>%
  group_by(line_id) %>%
  slice_max(token_id) %>%
  ungroup() %>%
  select(`Rhyme scheme` = rhyme_scheme)

# duplicate rs for each verse (since it fails for some verses)
rs <- slice(rs, rep(1:nrow(rs), verses))

# Initialize list to hold datatables
ve <- vector(mode = "list", length = no_tries)

options(DT.options = list(dom = 't', pageLength = 100, autoWidth = TRUE))

for (i in 1:no_tries) {
  set.seed(i)

  cutup <- salmer::annotated_hymns %>%
    cut_up(my_hymn, except = "PUNCT")
    # %>% filter(verse == 1)

  final <- cutup %>% new_rhymes(annotated_hymns)

  cutup_readable <- cutup %>%
    collapse_annotation(token_new) %>%
    select(`Cut-up` = text)

  final_readable <- final %>%
    collapse_annotation(token_new) %>%
    select(`Fixed rhymes` = text)

  # save text version of final output
  out <- bind_rows(tibble(`Fixed rhymes` = as.character(i)),
            final_readable,
            tibble(`Fixed rhymes` = NULL))

  for (x in seq(5, nrow(final_readable) + verses + 1, 5)) {
    out <- add_row(out, `Fixed rhymes` = "", .after = x)
  }

  write_csv2(out, file = "data-raw/out.csv", append = T)

  # show final cut-up version along with original and other bits
  comparison <- hymns %>%
    filter(doc_id == my_hymn) %>%
    select(Verse = verse, Original = text) %>%
    bind_cols(cutup_readable, final_readable, rs, i = i)

  ve[[i]] <- DT::datatable(comparison, rownames = F,
                         options = list(columnDefs = list(list(width = '10px', targets = c(0, 4)))))

}

# Show in data tables
1:no_tries %>% map(~ve[[.x]])


# Find rhymes
pr <- pronunciation %>%
  mutate(vowels = str_count(tolower(token), "a|e|i|o|u|y|æ|ø|å|é|ó|í"))

pr %>% filter(vowels == 3) %>% slice_sample(n = 10)

get_rhymes("gir", filter(pr, vowels >= 1))

get_rhymes("nærmes", filter(annotated_hymns, vowels >= 1))
