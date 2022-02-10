library(tidyverse)
library(devtools)
library(DT)
load_all()

# Get pronounciation dictionary
# pronounciation <- readRDS("data-raw/pronounciation.rds")

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
get_rhymes("borg", pronounciation)
get_rhymes("kartofler", pronounciation)
get_rhymes("smerte", pronounciation)
get_rhymes("svin", pronounciation)
get_rhymes("fisk", pronounciation)

# Find a great first verse
my_hymn <- 15

rs <- annotated_hymns %>%
   filter(doc_id == my_hymn, verse == 1, upos != "PUNCT") %>%
  group_by(line_id) %>%
  slice_max(token_id) %>%
  ungroup() %>%
  select(`Rhyme scheme` = rhyme_scheme)

ve <- list(10)

for (i in 1:10) {
  set.seed(i)

  cutup <- salmer::annotated_hymns %>%
    cut_up(my_hymn, except = "PUNCT") %>%
    filter(verse == 1)

  final <- cutup %>% new_rhymes(annotated_hymns)

  cutup_readable <- cutup %>%
    collapse_annotation(token_new) %>%
    select(`Cut-up` = text)

  final_readable <- final %>%
    collapse_annotation(token_new) %>%
    select(`Fixed rhymes` = text)

  # show final cut-up version along with original and other bits
  comparison <- hymns %>%
    filter(doc_id == my_hymn, verse == 1) %>%
    select(Verse = verse, Original = text) %>%
    bind_cols(cutup_readable, final_readable, rs)

  options(DT.options = list(dom = 't', pageLength = 100, autoWidth = TRUE))

  ve[[i]] <- DT::datatable(comparison, rownames = F,
                options = list(columnDefs = list(list(width = '10px', targets = c(0, 4)))))
  }


