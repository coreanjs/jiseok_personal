library(XML)
library(xml2)





################### rgcam

library(rgcam)


setwd("E:/gcam-v7.0-Windows-Release-Package")

conn <- localDBConn('./output', 'database_basexdb.0')

conn

prj <- addScenario(conn, 'jiseok_project_rgcam_test.dat', 'reference_with_dac_ssp_2')

str(prj)






## List scenarios and queries
scenarios <- listScenarios(prj)

scenarios

??listQueries
queries <- listQueries(, 'reference_with_dac_ssp_2', anyscen =TRUE)


## Retrieve query named "GDP by region" for all scenarios in the dataset,
## formatted as a single table
gdp.rgn <- getQuery(prj, 'GDP by region')




############### XML

setwd("C:/R/Rproject/jiseok_personal")

dac_ssp2<-read_xml("dac_ssp2.xml")
dac_ssp2_x3<-read_xml("dac_ssp2_x3.xml")


dac_ssp2

dac_ssp2_parse<-xmlParse("dac_ssp2.xml")

dac_ssp2_parse

dac_ssp2

library(httr)
??getURL

dac_sspe2_parse <-getURL("dac_ssp2.xml")
xmlToDataFrame(dac_ssp2_parse)


xmlToDataFrame(nodes = getNodeSet(dac_sspe2_parse, "//supplysector"))


##R에서 깔끔하게 XML 파일 데이터 프레임으로 만들기(feat. unnest_longer, unnest_wider)

library(tidyverse)

dac_ssp2

dac_ssp2 %>% 
  as_list() %>% 
  as_tibble() %>% 
  unnest(scenario) %>% 
  unnest(scenario) %>% 
  unnest_longer(scenario) %>% 
  unnest(everything())


## R에서 XML 태그 속성 parsing 하기
a<- xml_find_all(dac_ssp2, xpath=".//region") %>% 
  map(xml_attrs) %>%
  map_df(~as.list(.)) %>%
  mutate(TYPE = xml_find_all(dac_ssp2, './/region', ) %>%  xml_name)

is.data.frame(a) # data frame으로 변환 시켜주었으므로 TRUE 나올 것

a

unique(a$name)



dac_ssp2<-read_xml("dac_ssp2.xml")


node<-xml_children(read_xml("dac_ssp2.xml"))


View(node)

seq_along(node)

# xml to data frame
??temp_row

lapply(seq_along(node),
       function(x){
         temp_row <-xml_find_all(node[x], './*')
         tibble(
           idx= x,
           key = temp_row %>% xml_name(),
           value = temp_row %>% xml_text()
         ) %>% return()
       }
) %>% bind_rows()









??xml_find_all
library(tidyverse)
library(xml2)

xml_structure(read_xml("dac_ssp2.xml"))

              



dac_ssp2_raw


node <- xml_children(dac_ssp2_raw)

seq_along(node)

node

xml_find_all(node)


## 시도

## read_xml으로 불러오고
dac_ssp2_raw<- read_xml("dac_ssp2.xml")


##xml_find_all에서 xpath ="//name". 모든 element 확인
dac_ssp_all <-xml_find_all(dac_ssp2_raw, xpath="//supplysector")


##nodeset 96개개
dac_ssp_all




xml_structure(dac_ssp_all)


##
xml_text(dac_ssp_all)



document <- list(root = list(parent = list(child = list(1))))

document

xmlDoc <- xml2::as_xml_document(document)
xmlDoc
xml2::xml_structure(xmlDoc)




prod_df <- map_dfr(products, function(p_node) {
  list(".//id", ".//number", ".//city", ".//name", ".//value") %>%
    set_names(stringr::str_extract, "\\w+") %>%
    map(~xml_find_all(p_node, .)) %>%
    map(xml_text) %>%
    as_tibble()
})

prod_df

### How to find the differences between XML files in R


dac_ssp2_raw <-xmlParse("dac_ssp2.xml")
dac_ssp2_raw

dac_ssp2<-read_xml(xmlParse("dac_ssp2.xml"))
dac_ssp2_x3<-read_xml("dac_ssp2_x3.xml")

dac_ssp2_x3

test<- xmlTreeParse("dac_ssp2.xml")

test

class(test)

str(test, max.level = 3)

topxml<- xmlRoot(test)

str(topxml, max.level=1)

head(topxml, n = 1)

topxml_SApply<- xmlSApply(topxml, function(x) xmlSApply(x, xmlValue))

str(topxml_SApply)

library(kableExtra)
topxml_SApply[1:5, 1] %>% kable()


library(DT)
test %>% DT::datatable(options=list(pageLength=2))

# NOTE: this will not handle attributes
as_path_df <- function(x) {
  as_list(x) %>%
    unlist() %>%
    as.list() %>%
    as_tibble() %>%
    gather(key, val)
}


as_path_df(dac_ssp2)

