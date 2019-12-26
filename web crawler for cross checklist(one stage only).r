rm(list=ls()) # 清除過去的所有內容



###############################  整支程式只需設定以下3行  ############################### 

school_code="009"  # 學校代碼(例如台大=001,東海=009)
year=108  # 需要的年份(一次最多兩~三年,超過兩~三年會因時間過久被網頁擋)
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
    
    ID_node=html_nodes(css2,".number a")  # 准考證號的nodes
    ID=html_text(ID_node)  # 抓出考生准考證號
    
    # 該系未分組ID時執行以下指令
    if(length(ID)>0){
      
      ID_node=html_nodes(css2,".number a")  # 准考證號的nodes
      ID=html_text(ID_node)  # 抓出考生准考證號
      
      
      school_dept_node=html_nodes(css2,".text-left a") # 學校與科系的nodes
      school_dept=html_text(school_dept_node)  # 抓出通過第一階段的學校與科系
      obj_dept=school_dept[1]  # school_dept[1]為申請目標大學某科系(組)
      
      
      #Release_node=html_nodes(css2,".badge-primary")  # 放榜日期的nodes
      #Release=html_text(Release_node)  # 抓出各校系放榜日
      
      
      # 判別該考生通過的是東海某科系的第一階段
      index=ifelse(substr(school_dept,start=1,stop=nchar(school_dept[1]))==school_dept[1],1,0)
      # 對index做累加 , 同一位同學的number會相同
      number=cumsum(index)
      
      
      # 合併申請入學年份	准考證號	通過第一階段的目標校系	通過第一階段的全部學校科系
      x=cbind(year[i],ID[number],obj_dept,school_dept) 
      # 命名行的名稱
      dimnames(x)[[2]]=c("申請入學年份","准考證號","通過第一階段的目標校系","通過第一階段的全部學校科系")
      
      
      # 若該系有分組(即有第2層頁面)則執行以下指令
    }else{
      
      group_code_node=html_nodes(css2,".table-sm a")  # 該系分組代碼的nodes
      group_code=substr(html_text(group_code_node),1,4) # 抓出該系分組代碼
      
      x=data.frame() # 開一個資料空間
      for(k in 1:length(group_code)){
        
        css3=read_html(paste0("https://freshman.tw/cross/",year[i],"/",school_code,code[j],"_",group_code[k]))  # 要爬資料的網址
        
        
        ID_node=html_nodes(css3,".number a")  # 准考證號的nodes
        ID=html_text(ID_node)  # 抓出考生准考證號
        
        
        school_dept_node=html_nodes(css3,".text-left a") # 學校與科系的nodes
        school_dept=html_text(school_dept_node)  # 抓出通過第一階段的學校與科系
        obj_dept=school_dept[1]  # school_dept[1]為申請目標大學某科系(組)
        
        
        #Release_node=html_nodes(css3,".badge-primary")  # 放榜日期的nodes
        #Release=html_text(Release_node)  # 抓出各校系放榜日
        
        
        # 判別該考生通過的是東海某科系的第一階段
        index=ifelse(substr(school_dept,start=1,stop=nchar(school_dept[1]))==school_dept[1],1,0)
        # 對index做累加 , 同一位同學的number會相同
        number=cumsum(index)
        
        
        # 合併申請入學年份	准考證號	通過第一階段的目標校系	通過第一階段的全部學校科系
        s=cbind(year[i],ID[number],obj_dept,school_dept) 
        # 命名行的名稱
        dimnames(s)[[2]]=c("申請入學年份","准考證號","通過第一階段的目標校系","通過第一階段的全部學校科系")
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
write.csv(final_data,paste0(file_path,obj_school,year[1],"-",year[length(year)],"年通過第一階段結果.csv"),row.names=FALSE)



########################################### 針對資料進行轉置 ###########################################

#　安裝需要的package
#install.packages('dplyr')  
#install.packages('reshape2') 

#　載入需要的package
library(dplyr)
require(reshape2)

# 調整資料
clean_data=select(final_data,-c("通過第一階段的目標校系")) %>%  # 刪除不需要的欄位
  distinct(申請入學年份,准考證號,通過第一階段的全部學校科系) %>%  # 根據特定欄位移除重覆值
  group_by(准考證號) %>%  # 根據准考證號分組
  mutate(Num=1:n()) # 加入編號

#　重新定義clean_data為data.frame
clean_data=data.frame(clean_data)

#　針對資料進行轉置　(參考網址:http://blog.fens.me/r-transform/)
trans_data=reshape(clean_data,idvar="准考證號",timevar="Num",v.names="通過第一階段的全部學校科系",direction = "wide")

# 輸出csv檔 , 且不show出列的編號
write.csv(trans_data,paste0(file_path,obj_school,year[1],"-",year[length(year)],"年通過第一階段結果(轉置後).csv"),row.names=FALSE)

end=Sys.time() # 程式結束時間
time=end-start # 此程式所花時間
time



########## 如需輸出成xlsx檔 , 需先安裝JAVA並設定JAVA路徑 , 最後再執行以下code ##########

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-9.0.1')  # 設定JAVA路徑
#install.packages('xlsx')  # 安裝可輸出成xlsx檔的套裝
#library(xlsx) # 叫出可輸出成xlsx檔的套裝
# 輸出xlsx檔 , 且不show出列的編號  
#write.xlsx(trans_data,paste0(file_path,obj_school,year[1],"-",year[length(year)],"年通過第一階段結果(轉置後).xlsx"),row.names=FALSE)