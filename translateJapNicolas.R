library(dplyr)
library(openxlsx)
library(ggplot2)

#Translation of the coupon_list_train

#Opening the files
coupon_list_train=read.csv("coupon_list_train.csv", encoding="UTF-8", stringsAsFactors=FALSE)
coupon_purchased_train=read.csv("coupon_detail_train.csv", encoding="UTF-8", stringsAsFactors=FALSE)
coupon_visit_train=read.csv("coupon_visit_train.csv", encoding="UTF-8", stringsAsFactors=FALSE)
coupon_area_train=read.csv("coupon_area_train.csv", encoding="UTF-8", stringsAsFactors=FALSE)
user_list=read.csv("user_list.csv", encoding="UTF-8", stringsAsFactors=FALSE)

coupon_list_test=read.csv("coupon_list_test.csv", encoding="UTF-8", stringsAsFactors=FALSE)
coupon_area_test=read.csv("coupon_area_test.csv", encoding="UTF-8", stringsAsFactors=FALSE)


#Translation files
capsule_translate=read.xlsx("CAPSULE_TEXT_Translation.xlsx",rows=6:50,cols=3:4)
genre_translate=read.xlsx("CAPSULE_TEXT_Translation.xlsx",rows=6:50,cols=7:8)
large_area=read.xlsx("Translations_Region.xlsx",cols=5:6)
ken_name=read.xlsx("Translations_Region.xlsx",cols=7:8)
small_area=read.xlsx("Translations_Region.xlsx",cols=9:10)

#Do some translation by merging files and removing Jap column


coupon_list_train_en=merge(coupon_list_train,capsule_translate,by = "CAPSULE_TEXT")[,-1]
coupon_list_train_en=merge(coupon_list_train_en,genre_translate,by.x = "GENRE_NAME",by.y = "CAPSULE_TEXT")[,-1]
names(coupon_list_train_en)[23]="CAPSULE_NAME"
names(coupon_list_train_en)[24]="GENRE_NAME"

coupon_list_train_en=merge(coupon_list_train_en,large_area,by = "large_area_name")[,-1]
coupon_list_train_en=merge(coupon_list_train_en,ken_name,by = "ken_name")[,-1]
coupon_list_train_en=merge(coupon_list_train_en,small_area,by = "small_area_name")[,-1]
names(coupon_list_train_en)[22]="large_area_name"
names(coupon_list_train_en)[23]="ken_name"
names(coupon_list_train_en)[24]="small_area_name"

coupon_purchased_train_en=merge(coupon_purchased_train,small_area,by.x = "SMALL_AREA_NAME",by.y = "small_area_name")[,-1]
names(coupon_purchased_train_en)[6]="small_area_name"

#Save translation
write.csv(coupon_list_train_en,"coupon_list_train_en_all.csv")
write.csv(coupon_purchased_train_en,"coupon_purchased_train_en.csv")


user_list_en=merge(user_list,ken_name,by.x = "PREF_NAME",by.y = "ken_name")[,-1]
names(user_list_en)[6]="ken_name"
names(user_list_en)

user_list_space=subset(user_list,PREF_NAME=="")
names(user_list_space)[5]="ken_name"
View(user_list_en_all)

user_list_en=rbind(user_list_en,user_list_space)

write.csv(user_list_en,"user_list_en.csv")

coupon_area_train_en=merge(coupon_area_train,ken_name,by.x = "PREF_NAME",by.y = "ken_name")[,-1]
names(coupon_area_train_en)[3]="ken_name"
coupon_area_train_en=merge(coupon_area_train_en,small_area,by.x = "SMALL_AREA_NAME",by.y = "small_area_name")[,-1]
names(coupon_area_train_en)[3]="small_area_name"
write.csv(coupon_area_train_en,"coupon_area_train_en.csv")


coupon_area_test_en=merge(coupon_area_test,ken_name,by.x = "PREF_NAME",by.y = "ken_name")[,-1]
names(coupon_area_test_en)[3]="ken_name"
coupon_area_test_en=merge(coupon_area_test_en,small_area,by.x = "SMALL_AREA_NAME",by.y = "small_area_name")[,-1]
names(coupon_area_test_en)[3]="small_area_name"
View(coupon_area_test_en)
write.csv(coupon_area_test_en,"coupon_area_test_en.csv")


coupon_list_test_en=merge(coupon_list_test,capsule_translate,by = "CAPSULE_TEXT")[,-1]
coupon_list_test_en=merge(coupon_list_test_en,genre_translate,by.x = "GENRE_NAME",by.y = "CAPSULE_TEXT")[,-1]
names(coupon_list_test_en)[23]="CAPSULE_NAME"
names(coupon_list_test_en)[24]="GENRE_NAME"

coupon_list_test_en=merge(coupon_list_test_en,large_area,by = "large_area_name")[,-1]
coupon_list_test_en=merge(coupon_list_test_en,ken_name,by = "ken_name")[,-1]
coupon_list_test_en=merge(coupon_list_test_en,small_area,by = "small_area_name")[,-1]
names(coupon_list_test_en)[22]="large_area_name"
names(coupon_list_test_en)[23]="ken_name"
names(coupon_list_test_en)[24]="small_area_name"
View(coupon_list_test_en)
write.csv(coupon_list_test_en,"coupon_list_test_en.csv")

