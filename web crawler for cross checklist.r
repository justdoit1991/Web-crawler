rm(list=ls()) # 清除過去的所有內容



###############################  整支程式只需設定以下3行  ############################### 

school_code="009"  # 學校代碼(例如台大=001,東海=009)
year=100:108  # 需要的年份(一次最多兩~三年,超過兩~三年會因時間過久被網頁擋)
file_path="C:\\Users\\User\\Desktop\\個人申請-交叉查榜\\" # 設定檔案輸出路徑(使用不同電腦需用不同路徑)

###############################  整支程式只需設定以上3行  ############################### 



#install.packages('rvest')  # 安裝爬蟲所需的套裝
#install.packages('stringr') # 安裝清除字串前後空白所需的套裝

library(rvest)  # 叫出爬蟲所需的套裝
library(stringr) # 叫出清除字串前後空白所需的套裝

start=Sys.time() # 程式開始時間

final_data=data.frame() # 開一個資料空間
for(i in 1:length(year)){
  
  css=read_html(paste0("https://freshman.tw/cross/",year[i],"/",school_code))  # 要爬資料的網址
  
  obj_school_code_node=html_nodes(css,"b")  # 目標大學校名&代碼的nodes
  obj_school_code=html_text(obj_school_code_node)  # 目標大學校名&代碼
  obj_school=substr(obj_school_code,start=regexpr(")",obj_school_code)+2,stop=nchar(obj_school_code))  # 目標大學校名
  
  all_code_node=html_nodes(css,"td:nth-child(1)")  # 目標大學校系代碼的nodes
  all_code=html_text(all_code_node)  # 抓出目標大學所有校系代碼
  code=substr(all_code,4,6)  # 抓出所有科系代碼
  
  data=data.frame() # 開一個資料空間
  for(j in 1:length(code)){
    
    css2=read_html(paste0("https://freshman.tw/cross/",year[i],"/",school_code,code[j]))  # 要爬資料的網址
    
    ID_area_city_node=html_nodes(css2,".showPhoto td:nth-child(2)")  # 准考證號和考區和來自縣市的nodes
    ID_area_city=html_text(ID_area_city_node)  # 抓出考生准考證號和考區和來自縣市
    
    # 該系未分組時執行以下指令
    if(length(ID_area_city)>0){
      
      ID_area_city_node=html_nodes(css2,".showPhoto td:nth-child(2)")  # 准考證號和考區和來自縣市的nodes
      ID_area_city=html_text(ID_area_city_node)  # 抓出考生准考證號和考區和來自縣市
      ID=substr(ID_area_city,start=1,stop=8)  # 篩選出考生准考證號
      area=ifelse(nchar(ID_area_city)==8,"",substr(ID_area_city,start=9,stop=10))  # 篩選出考生的考區
      city=ifelse(nchar(ID_area_city)==8,"",substr(ID_area_city,start=12,stop=13))  # 篩選出考生的來自縣市
      
      
      name_number_node=html_nodes(css2,"td:nth-child(3)")  # 考生姓名和報名幾間學校的nodes
      name_number=html_text(name_number_node)  # 抓出考生姓名和報名幾間學校
      name=str_replace_all(name_number,"[0-9]+","")  # 將考生姓名後的數字全取代為"" , 只留下姓名
      index=str_extract(name_number,"[0-9]+") # 找出考生姓名後的數字=報名幾間學校
      number=rep(1:length(name),index)  #  利用length計算報名的同學個數 , 再利用rep讓同一位同學的number相同
      
      
      school_related_node=html_nodes(css2,".text-left,.crown") # 學校相關的nodes
      school_related=html_text(school_related_node)  # 抓出申請的學校與科系
      
      aa=ifelse(school_related=="",1,0)  # 當school_related=""則前一所學校即為就讀學校 , 先判別""的所在位置
      bb=c(aa[2:length(aa)],0)  # 再將所有指標往前平移一位
      cc=cbind(school_related,bb) # 合併學校和指標
      dd=cc[cc[,1]!="",]  # 將空值排除
     
      school_dept=dd[,1] # 考生申請的學校與科系
      choice=dd[,2] # 考生的就讀意願
      
      # 抓出申請的校名
      school=ifelse(regexpr("大學",school_dept)>0,substr(school_dept,start=1,stop=regexpr("大學",school_dept)+1)
                    ,ifelse(regexpr("學院",school_dept)>0,substr(school_dept,start=1,stop=regexpr("學院",school_dept)+1),""))
      # 抓出申請的系名
      dept=str_trim(ifelse(regexpr("大學",school_dept)>0,substr(school_dept,start=regexpr("大學",school_dept)+2,stop=nchar(school_dept))
                           ,ifelse(regexpr("學院",school_dept)>0,substr(school_dept,start=regexpr("學院",school_dept)+2,stop=nchar(school_dept)),"")))
      # dept[1]為申請目標大學某科系(組)
      obj_dept=dept[1]  
      
      
      list_node=html_nodes(css2,".text-left+ td")  # 正備取和名次的nodes
      list=html_text(list_node)  # 抓出正備取和名次
      
      # 將正備取的名次取代為空值 = 只留下正取或備取
      admission=str_replace_all(list,"[0-9]+","")
      # 如果正備取包含"取"字則排除
      admission=ifelse(regexpr("取",admission)>0,substr(admission,start=1,stop=nchar(admission)-1),admission)
      # 抓出正備取後的字即為名次
      rank=substr(list,start=nchar(admission)+1,stop=nchar(list))
      
      
      # 合併申請入學年份 准考證號 考生來自考區 考生來自縣市 姓名 申請目標大學的科系 申請的學校  申請的科系	正備取  名次  最後是否去該校
      x=cbind(year[i],ID[number],area[number],city[number],name[number],obj_dept,school,dept,admission,rank,choice) 
      # 命名行的名稱
      dimnames(x)[[2]]=c("申請入學年份","准考證號","考生來自考區","考生來自縣市","姓名",paste0("申請",obj_school,"的科系"),"有申請的全部學校","有申請的全部科系","正備取","名次","最後是否去該校")
      
      # 若該系有分組(即有第2層頁面)則執行以下指令
    }else{
      
      group_code_node=html_nodes(css2,".table-sm a")  # 該系分組代碼的nodes
      group_code=substr(html_text(group_code_node),1,4) # 抓出該系分組代碼
      
      x=data.frame() # 開一個資料空間
      for(k in 1:length(group_code)){
        
        css3=read_html(paste0("https://freshman.tw/cross/",year[i],"/",school_code,code[j],"_",group_code[k]))  # 要爬資料的網址
        
        
        ID_area_city_node=html_nodes(css3,".showPhoto td:nth-child(2)")  # 准考證號和考區和來自縣市的nodes
        ID_area_city=html_text(ID_area_city_node)  # 抓出考生准考證號和考區和來自縣市
        ID=substr(ID_area_city,start=1,stop=8)  # 篩選出考生准考證號
        area=ifelse(nchar(ID_area_city)==8,"",substr(ID_area_city,start=9,stop=10))  # 篩選出考生的考區
        city=ifelse(nchar(ID_area_city)==8,"",substr(ID_area_city,start=12,stop=13))  # 篩選出考生的來自縣市
        
        
        name_number_node=html_nodes(css3,"td:nth-child(3)")  # 考生姓名和報名幾間學校的nodes
        name_number=html_text(name_number_node)  # 抓出考生姓名和報名幾間學校
        name=str_replace_all(name_number,"[0-9]+","")  # 將考生姓名後的數字全取代為"" , 只留下姓名
        index=str_extract(name_number,"[0-9]+") # 找出考生姓名後的數字=報名幾間學校
        number=rep(1:length(name),index)  #  利用length計算報名的同學個數 , 再利用rep讓同一位同學的number相同
        
        
        school_related_node=html_nodes(css3,".text-left,.crown") # 學校相關的nodes
        school_related=html_text(school_related_node)  # 抓出申請的學校與科系
        
        aa=ifelse(school_related=="",1,0)  # 當school_related=""則前一所學校即為就讀學校 , 先判別""的所在位置
        bb=c(aa[2:length(aa)],0)  # 再將所有指標往前平移一位
        cc=cbind(school_related,bb) # 合併學校和指標
        dd=cc[cc[,1]!="",]  # 將空值排除
        
        if(is.matrix(dd)==TRUE)
        {
          school_dept=dd[,1] # 考生申請的學校與科系  
          choice=dd[,2] # 考生的就讀意願
        }else{
          school_dept=dd[1] # 考生申請的學校與科系(針對只有一筆資料時)
          choice=dd[2] # 考生的就讀意願(針對只有一筆資料時)
        }

        # 抓出申請的校名
        school=ifelse(regexpr("大學",school_dept)>0,substr(school_dept,start=1,stop=regexpr("大學",school_dept)+1)
                      ,ifelse(regexpr("學院",school_dept)>0,substr(school_dept,start=1,stop=regexpr("學院",school_dept)+1),""))
        # 抓出申請的系名
        dept=str_trim(ifelse(regexpr("大學",school_dept)>0,substr(school_dept,start=regexpr("大學",school_dept)+2,stop=nchar(school_dept))
                             ,ifelse(regexpr("學院",school_dept)>0,substr(school_dept,start=regexpr("學院",school_dept)+2,stop=nchar(school_dept)),"")))
        # dept[1]為申請目標大學某科系(組)
        obj_dept=dept[1]  
        
        
        list_node=html_nodes(css3,".text-left+ td")  # 正備取和名次的nodes
        list=html_text(list_node)  # 抓出正備取和名次
        
        # 將正備取的名次取代為空值 = 只留下正取或備取
        admission=str_replace_all(list,"[0-9]+","")
        # 如果正備取包含"取"字則排除
        admission=ifelse(regexpr("取",admission)>0,substr(admission,start=1,stop=nchar(admission)-1),admission)
        # 抓出正備取後的字即為名次
        rank=substr(list,start=nchar(admission)+1,stop=nchar(list))

        
        # 合併申請入學年份 准考證號 考生來自考區 考生來自縣市 姓名 申請目標大學的科系 申請的學校  申請的科系	正備取  名次  最後是否去該校
        s=cbind(year[i],ID[number],area[number],city[number],name[number],obj_dept,school,dept,admission,rank,choice) 
        # 命名行的名稱
        dimnames(s)[[2]]=c("申請入學年份","准考證號","考生來自考區","考生來自縣市","姓名",paste0("申請",obj_school,"的科系"),"有申請的全部學校","有申請的全部科系","正備取","名次","最後是否去該校")
        # 合併該系所有分組的資料
        x=rbind(x,s)
      }
    }
    # 合併各系資料
    data=rbind(data,x)
  }
  # 合併各年資料
  final_data=rbind(final_data,data)
}

# 輸出csv檔 , 且不show出列的編號
write.csv(final_data,paste0(file_path,obj_school,year[1],"-",year[length(year)],"年交叉查榜結果.csv"),row.names=FALSE)

end=Sys.time() # 程式結束時間
time=end-start # 此程式所花時間
time


########## 如需輸出成xlsx檔 , 需先安裝JAVA並設定JAVA路徑 , 最後再執行以下code ##########

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-9.0.1')  # 設定JAVA路徑
#install.packages('xlsx')  # 安裝可輸出成xlsx檔的套裝
#library(xlsx) # 叫出可輸出成xlsx檔的套裝
# 輸出xlsx檔 , 且不show出列的編號  
#write.xlsx(final_data,paste0(file_path,obj_school,year[1],"-",year[length(year)],"年交叉查榜結果.xlsx"),row.names=FALSE)