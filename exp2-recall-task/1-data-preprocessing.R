## preprocessing and cleaning recall task data


library(tidyverse)
library(openxlsx)

#### Clean Qualtrics data ------------------------------------------------------

## load Qualtrics data
asp <- read_csv("data/raw/recall-task-raw-10Nov2021.csv", col_names = TRUE, skip = 1)

## get rid of timing variables and other unwanted columns
asp <- asp %>% 
  select(-contains("Timing")) %>% 
  filter(Progress == 100) %>% 
  select(-c("Start Date":"User Language", "PROLIFIC_PID", "CompletionID"))

## rename column headings
asp <- asp %>% rename('age' = 'العمر:',
                      'gender' = 'الجنس:',
                      'ethnicity' = 'الأصل العرقي:',
                      'race' = 'عرقك:',
                      'place_of_birth' = 'مكان الميلاد:',
                      'grew_up' = 'حيث نشأت:',
                      'nat_language' = 'اللغة الأم (يرجى ملاحظة أنه سيتم الدفع لك مقابل هذه التجربة بغض النظر عن ردك): - Selected Choice',
                      'language_other' = 'اللغة الأم (يرجى ملاحظة أنه سيتم الدفع لك مقابل هذه التجربة بغض النظر عن ردك): - غير ذلك (يرجى التحديد) - Text',
                      'language_experience' = 'اللغات الأخرى التي يتم التحدث بها (يرجى وصف المكان الذي تم تعلمه ومدته، ودرجة الطلاقة / معرفة القراءة والكتابة، وعدد مرات استخدامها):',
                      'prolific_ID' = 'Prolific ID:',
                      'comments' = 'أي أسئلة أو تعليقات؟')

## rename stim column headings
asp <- asp %>% 
  rename('SRC45' = contains("...632")) %>% 
  rename('SRC44' = contains("...627")) %>% 
  rename('SRC43' = contains("...622")) %>% 
  rename('SRC42' = contains("...617")) %>% 
  rename('SRC41' = contains("...612")) %>% 
  rename('SRC40' = contains("...607")) %>% 
  rename('SRC38' = contains("...602")) %>% 
  rename('SRC37' = contains("...597")) %>% 
  rename('SRC36' = contains("...592")) %>% 
  rename('SRC35' = contains("...587")) %>% 
  rename('ORC23' = contains("...582")) %>% 
  rename('ORC22' = contains("...577")) %>% 
  rename('ORC21' = contains("...572")) %>% 
  rename('ORC20' = contains("...567")) %>% 
  rename('ORC19' = contains("...562")) %>% 
  rename('ORC18' = contains("...557")) %>% 
  rename('ORC16' = contains("...552")) %>% 
  rename('ORC15' = contains("...547")) %>% 
  rename('ORC14' = contains("...542")) %>% 
  rename('ORC13' = contains("...537")) %>% 
  rename('SRC34' = contains("...532")) %>% 
  rename('SRC33' = contains("...527")) %>% 
  rename('SRC32' = contains("...522")) %>% 
  rename('SRC31' = contains("...517")) %>% 
  rename('SRC30' = contains("...512")) %>% 
  rename('SRC29' = contains("...507")) %>% 
  rename('SRC28' = contains("...502")) %>% 
  rename('SRC27' = contains("...497")) %>% 
  rename('SRC25' = contains("...492")) %>% 
  rename('SRC24' = contains("...487")) %>% 
  rename('ORC11' = contains("...482")) %>% 
  rename('ORC10' = contains("...477")) %>% 
  rename('ORC9' = contains("...472")) %>% 
  rename('ORC8' = contains("...467")) %>% 
  rename('ORC7' = contains("...462")) %>% 
  rename('ORC6' = contains("...457")) %>% 
  rename('ORC5' = contains("...452")) %>% 
  rename('ORC4' = contains("...447")) %>% 
  rename('ORC2' = contains("...442")) %>% 
  rename('ORC1' = contains("...437")) %>% 
  rename('F85' = contains("...432")) %>% 
  rename('F84' = contains("...427")) %>% 
  rename('F83' = contains("...422")) %>% 
  rename('F82' = contains("...417")) %>% 
  rename('F81' = contains("...412")) %>% 
  rename('F80' = contains("...407")) %>% 
  rename('F79' = contains("...402")) %>% 
  rename('F78' = contains("...397")) %>% 
  rename('F77' = contains("...392")) %>% 
  rename('F76' = contains("...387")) %>% 
  rename('F75' = contains("...382")) %>% 
  rename('F74' = contains("...377")) %>% 
  rename('F73' = contains("...372")) %>% 
  rename('F72' = contains("...367")) %>% 
  rename('F71' = contains("...362")) %>% 
  rename('F70' = contains("...357")) %>% 
  rename('F69' = contains("...352")) %>% 
  rename('F68' = contains("...347")) %>% 
  rename('F67' = contains("...342")) %>% 
  rename('F66' = contains("...337")) %>% 
  rename('ORC45' = contains("...332")) %>% 
  rename('ORC44' = contains("...327")) %>% 
  rename('ORC43' = contains("...322")) %>% 
  rename('ORC42' = contains("...317")) %>% 
  rename('ORC41' = contains("...312")) %>% 
  rename('ORC40' = contains("...307")) %>% 
  rename('ORC38' = contains("...302")) %>% 
  rename('ORC37' = contains("...297")) %>% 
  rename('ORC36' = contains("...292")) %>% 
  rename('ORC35' = contains("...287")) %>% 
  rename('SRC23' = contains("...282")) %>% 
  rename('SRC22' = contains("...277")) %>% 
  rename('SRC21' = contains("...272")) %>% 
  rename('SRC20' = contains("...267")) %>% 
  rename('SRC19' = contains("...262")) %>% 
  rename('SRC18' = contains("...257")) %>% 
  rename('SRC16' = contains("...252")) %>% 
  rename('SRC15' = contains("...247")) %>% 
  rename('SRC14' = contains("...242")) %>% 
  rename('SRC13' = contains("...237")) %>% 
  rename('F65' = contains("...232")) %>% 
  rename('F64' = contains("...227")) %>% 
  rename('F63' = contains("...222")) %>% 
  rename('F62' = contains("...217")) %>% 
  rename('F61' = contains("...212")) %>% 
  rename('F60' = contains("...207")) %>% 
  rename('F59' = contains("...202")) %>% 
  rename('F58' = contains("...197")) %>% 
  rename('F57' = contains("...192")) %>% 
  rename('F56' = contains("...187")) %>% 
  rename('F55' = contains("...182")) %>% 
  rename('F54' = contains("...177")) %>% 
  rename('F53' = contains("...172")) %>% 
  rename('F52' = contains("...167")) %>% 
  rename('F51' = contains("...162")) %>% 
  rename('F50' = contains("...157")) %>% 
  rename('F49' = contains("...152")) %>% 
  rename('F48' = contains("...147")) %>% 
  rename('F47' = contains("...142")) %>% 
  rename('F46' = contains("...137")) %>% 
  rename('ORC34' = contains("...132")) %>% 
  rename('ORC33' = contains("...127")) %>% 
  rename('ORC32' = contains("...122")) %>% 
  rename('ORC31' = contains("...117")) %>% 
  rename('ORC30' = contains("...112")) %>% 
  rename('ORC29' = contains("...107")) %>% 
  rename('ORC28' = contains("...102")) %>% 
  rename('ORC27' = contains("...97")) %>% 
  rename('ORC25' = contains("...92")) %>% 
  rename('ORC24' = contains("...87")) %>% 
  rename('SRC11' = contains("...82")) %>% 
  rename('SRC10' = contains("...77")) %>% 
  rename('SRC9' = contains("...72")) %>% 
  rename('SRC8' = contains("...67")) %>% 
  rename('SRC7' = contains("...62")) %>% 
  rename('SRC6' = contains("...57")) %>% 
  rename('SRC5' = contains("...52")) %>% 
  rename('SRC4' = contains("...47")) %>% 
  rename('SRC2' = contains("...42")) %>% 
  rename('SRC1' = contains("...37")) %>% 
  rename('P' = contains("...32"))

## remove select participants
asp <- asp %>% 
  filter(!(prolific_ID == "614e1d9272b5f46d5946f49e") & 
          !(prolific_ID == "سيشب") & 
          !(prolific_ID == "5c50a48b92acd60001b26b56"))



#### Subset data ---------------------------------------------------------------

participant_data <- asp %>% 
  select(c(prolific_ID:language_experience, comments))

recall_data <- asp %>% 
  select(-c(age:language_experience, comments))



#### Clean recall data ---------------------------------------------------------

recall_data <- recall_data %>% 
  pivot_longer(P:SRC45, names_to = "stim", values_to = "recall") %>% 
  drop_na(recall)

## next step: code if participant got it correct or not/included resumptive 
## pronoun clitic or not

# write.xlsx(recall_data, "data/clean/recall-data-10Nov2021.xlsx")
# write.xlsx(participant_data, "data/clean/participant-data-10Nov2021.xlsx")

