;***********************定义数据段代码***************************************
DATAS SEGMENT
SHOW DB 0AH,0DH
	 DB '*******************************************************************',0AH,0DH
	 DB 0AH,0DH 
     DB '************this is a music program! please select!****************',0AH,0DH
     DB 0AH,0DH  
     DB '*******************************************************************',0AH,0DH
	 DB 0AH,0DH
	 DB 'In this part, you can press below keys to enjoy music!',0AH,0DH
	 DB 'Song lists:',0AH,0DH
     DB '1. bei jing huan ying ni',0AH,0DH
     DB '2. chun tian hua hui kai',0AH,0DH
     DB '3. huan le song',0AH,0DH
     DB '4. dan yuan ren chang jiu',0AH,0DH
     DB '5. qing fei de yi',0AH,0DH
     DB 'You can press',60H,'Esc',27H,' or ',60H,'Enter',27H, ' to exit.',0AH,0DH 
     DB 0AH,0DH
     DB 'During the song you can press below keys to set the speed and playback mode:',0AH,0DH 
     DB 'u -> Faster',0AH,0DH
     DB 'd -> Slower',0AH,0DH
     DB 'p -> Pause',0AH,0DH
     DB 's -> Start',0AH,0DH
     DB 'q -> Quit',0AH,0DH
     DB '< -> Previous',0AH,0DH
     DB '> -> Next',0AH,0DH,'$'
M1   DB 'bei jing huan ying ni',0AH,0DH,'$'
M2   DB 'chun tian hua hui kai',0AH,0DH,'$'
M3   DB 'huan le song',0AH,0DH,'$'
M4   DB 'dan yuan ren chang jiu',0AH,0DH,'$'
M5   DB 'qing fei de yi',0AH,0DH,'$'
WAIT_M  DB 0AH,0DH
        DB 'music palying','...',0AH,0DH,'$'	
ERROR_O DB 0AH,0DH,'Your order is error, please enter the right one:$'
Q DB 0AH,0DH,'The song is over, you can choose a song again.$'
P DB 0AH,0DH,'Have a rest, you can press s to start the song again.$'
S DB 0AH,0DH,'You has start the song again.',0AH,0DH,'$'
U DB 0AH,0DH,'You have made the beat shorter.',0AH,0DH,'$'
D DB 0AH,0DH,'You have made the beat longer.',0AH,0DH,'$'
OVER DB 0AH,0DH,'***************************************$' 
     DB 0AH,0DH,'this is a music program! please select!$' 
     DB 0AH,0DH,'***************************************$'

;****************************北京欢迎你***************************** 
TONES_1 DW 659,2 DUP (659,784,659,587,659,587,659,659,587,440,523,659,587,587,523,440,523,587,659,784,587,659,880,784,440,587,523) 
        DW 587,523,440,523,587,659,784,587,659,880,784,784,659,587,659,587,523,784,880,587,440,659,587,587,523,-1
BEAT_1  DW 7 DUP(25*200),50*200,5 DUP(25*200),25*600,13 DUP(25*200),25*600,6 DUP(25*200),50*200,5 DUP(25*200),25*600,13 DUP(25*200),25*600
        DW 12 DUP(25*200),50*400,6 DUP(25*200),50*200,4 DUP(25*200),50*200 
        
;**************************春天花会开******************************
TONES_2 DW 392,392,392,392,440,392,330,294,262,262,262,220,294  
        DW 392,392,392,392,330,494,220,262,262,262,330,294,330,440,220,262,-1
BEAT_2  DW 25*200,3 DUP(25*200,50*200,25*200,50*200,50*200,50*400)
        DW 25*200,50*200,25*200,50*200,50*200,50*200,50*200,4 DUP(25*200)

;****************************欢乐颂*****************************
TONES_3 DW 330,330,330,349,392,392,349,330,294,262,262,294,330,330,294,294
        DW 330,330,349,392,392,349,330,294,262,262,294,330,294,262,262,-1
BEAT_3  DW 13 DUP(50*200),25*600,2 DUP(25*200)
        DW 12 DUP(50*200),25*600,2 DUP(25*200)

;**************************但愿人长久******************************
TONES_4 DW 262,262,262,220,196,220,262,262,262,220,196,220,294  
        DW 330,262,220,330,262,220,294,262,220,349,349,220,262,294,294,196,247,220,247,262,-1
BEAT_4  DW 100,2 DUP(50*200,25*200,25*200,25*600,25*200,50*400)
        DW 2 DUP(50*200,25*200,25*200),25*600,25*200,50*400,50*200,50*200,25*200,25*600,50*400,50*200,50*200,25*200,50*200,25*200,50*400
         
;***************************情非得已*****************************
TONES_5 DW 592,592,523,587,659,587,587
DW  4 DUP(523),784,784,659,784,3 DUP(659),587,659,698,659,659  
         DW 659,587,523,523,440,440,523,587
DW 4 DUP(392),440,523,523,440,659,523,523,587,587,523,523,-1
BEAT_5 DW 12 DUP(25*200),50*600,6 DUP(25*200),50*200,25*200,25*200,50*600
        DW 6 DUP(25*200),50*200,25*200,25*200,50*600,12 DUP(25*200),50*600
BEAT DW ?
EXIT_BOX DB ?
SIDI DW 4 DUP(?)
SITE DB 30H
DATAS ENDS
;数据段定义

STACKS     SEGMENT  

STACKS     ENDS
;堆栈段定义
 
CODES SEGMENT
    ASSUME CS:CODES,DS:DATAS,SS:STACKS
MAIN PROC FAR
    MOV AX,DATAS
    MOV DS,AX
    LEA SI,SHOW
    CALL DISPLAY          ;显示菜单列表
    CALL FAR PTR INPUT      
DONE:
	LEA SI,OVER
	CALL DISPLAY      ;显示结束菜单
MAIN ENDP

;************显示菜单列表*************
DISPLAY PROC NEAR
	MOV DX,SI
   MOV AH,09H            
   INT 21H	
RET
DISPLAY ENDP

INPUT PROC FAR
	
	PUSH AX
	MOV AH,01H
    INT 21H                 ;输入字符
	CMP AL,0DH
	JZ TZ
	CMP AL,1BH
	JZ TZ
TAP:	
	LEA BX,BEAT
    MOV [BX],word ptr 28010 
    LEA BX,EXIT_BOX 
    MOV BYTE PTR [BX],0  
	MOV SITE,AL

	CMP AL,'1'              ;判断输入选择歌曲1,2,3,4,5
	JZ SONG_1
	CMP AL,'2'
	JZ SONG_2
	CMP AL,'3'
	JZ SONG_3
	CMP AL,'4'
	JZ SONG_4
	CMP AL,'5'
	JZ TZ2
	JMP INPUT
AA:
	MOV AL,SITE
	JMP TAP
SONG_1:
	LEA SI,WAIT_M           ;提示音乐播放中
	CALL DISPLAY
	LEA SI,M1
	CALL DISPLAY
	LEA SI,TONES_1          
	LEA DI,BEAT_1           ;导入相应乐谱，音节存放地址
	MOV SIDI,SI
	MOV SIDI+2,DI           ;用SIDI记录存放地址
	CALL FAR PTR PLAY_MUSIC         ;开启音乐播放程序
	CMP BYTE PTR[BX],4
	JZ  AA
	JMP INPUT  
TZ:	JMP EXIT             
SONG_2:
	LEA SI,WAIT_M           ;提示音乐播放中
	CALL DISPLAY
	LEA SI,M2
	CALL DISPLAY
	LEA SI,TONES_2
	LEA DI,BEAT_2           ;导入相应乐谱，音节存放地址
	MOV SIDI,SI
	MOV SIDI+2,DI           ;用SIDI记录存放地址
	CALL FAR PTR PLAY_MUSIC         ;开启音乐播放程序
	CMP BYTE PTR[BX],4
	JZ  AA
	JMP INPUT
TZ2:	JMP SONG_5
SONG_3:
	LEA SI,WAIT_M           ;提示音乐播放中
	CALL DISPLAY
	LEA SI,M3
	CALL DISPLAY
	LEA SI,TONES_3
	LEA DI,BEAT_3           ;导入相应乐谱，音节存放地址
	MOV SIDI,SI
	MOV SIDI+2,DI           ;用SIDI记录存放地址           
	CALL FAR PTR PLAY_MUSIC         ;开启音乐播放程序
	CMP BYTE PTR[BX],4
	JZ  AA
	JMP INPUT  
SONG_4:
	LEA SI,WAIT_M           ;提示音乐播放中
	CALL DISPLAY
	LEA SI,M4
	CALL DISPLAY
	LEA SI,TONES_4
	LEA DI,BEAT_4           ;导入相应乐谱，音节存放地址
	MOV SIDI,SI
	MOV SIDI+2,DI           ;用SIDI记录存放地址 
	CALL FAR PTR PLAY_MUSIC         ;开启音乐播放程序
	CMP BYTE PTR[BX],4
	JZ  AA
	JMP INPUT 
SONG_5:
	LEA SI,WAIT_M           ;提示音乐播放中
	CALL DISPLAY
	LEA SI,M5
	CALL DISPLAY
	LEA SI,TONES_5
	LEA DI,BEAT_5           ;导入相应乐谱，音节存放地址
	MOV SIDI,SI
	MOV SIDI+2,DI           ;用SIDI记录存放地址 
	CALL FAR PTR PLAY_MUSIC         ;开启音乐播放程序
	CMP BYTE PTR[BX],4
	JZ  AA
	JMP INPUT
EXIT:
	POP AX                  ;栈顶内容取出并存到目的操作数中，同时修正堆栈指针
	MOV AH,4CH
    INT 21H                     ;返回DOS系统
	RET
INPUT ENDP

;**************音乐播放子函数***************
PLAY_MUSIC PROC FAR
AGAIN:
	CMP [SI],word ptr 0FFFFH               ;判断一段音乐是否结束
	JZ CIRCLE                     ;若结束,则单曲循环
	CALL FAR PTR KEY_C                    ;按键中断运行
	LEA BX,EXIT_BOX
	CMP BYTE PTR[BX],4
	JZ RETURN
	CMP BYTE PTR[BX],3
	JZ ERROR
	CMP BYTE PTR[BX],2
	JZ RETURN
    CMP BYTE PTR[BX],1
    JZ AGAIN
    CMP BYTE PTR[BX],0
    JZ NEXT
ERROR:                                ;错误控制
	PUSH SI
	LEA SI,ERROR_O
	CALL DISPLAY
	POP SI
	JMP RETURN	    	
NEXT:                             
	MOV AL,0B6H                   ;计数器的控制字要写入控制寄存器
	OUT 43H,AL                    ;输入控制字寄存器,选用方式2,工作在方式三
	MOV DX,12H                    ;把给定频率的高八位送给DX
	MOV AX,3280H                  ;把给定频率的低八位送给AX
	DIV WORD PTR[SI]              ;SI的内容为给定的频率，放入
	OUT 42H,AL                    ;低八位写入计数器二
	MOV AL,AH                     ;将高八位内容送到AL
	OUT 42H,AL                    ;用两次计数器通道2用于发声
	
	CALL SPK_ON                   ;启动蜂鸣器
	MOV CX,[DI]
BEAT_M:                               ;节拍控制
	PUSH CX
	LEA BX,BEAT
	MOV CX,[BX]
DELAY_M:                              ;延时控制
	LOOP DELAY_M
	POP CX
	LOOP BEAT_M
	INC SI
	INC SI
	INC DI
	INC DI
	CALL SPK_OFF                  ;关闭蜂鸣器
	JMP AGAIN
CIRCLE:
	MOV SI,SIDI
	MOV DI,SIDI+2                 ;将其地址送回SI、DI
	JMP PLAY_MUSIC	
RETURN:
	RET                           ;返回系统
PLAY_MUSIC ENDP

;***************按键中断子函数*************
KEY_C PROC FAR
	PUSH AX
	MOV AH,0BH
	INT 21H
	XOR AH,AH	
	CMP AL,0FFH
	JZ OK
	JMP OUT_K
	
OK:	MOV AH,01H
	INT 21H                       ;输入
	
	XOR AH,AH
	CMP AL,0DH
	JZ QUIT
	
	XOR AH,AH
	CMP AL,1BH
	JZ QUIT
	                               ;判断输入选择功能
	XOR AH,AH
	CMP AL,'q'                     ;退出功能
	JZ QUIT
	CMP AL,'Q'
	JZ QUIT
	
	XOR AH,AH                      ;加速功能
	CMP AL,'u'
	JZ UP
	CMP AL,'U'
	JZ UP
	
	XOR AH,AH                      ;减速功能
	CMP AL,'d'
	JZ DOWN
	CMP AL,'D'
	JZ DOWN
	    
	XOR AH,AH                       ;暂停功能
	CMP AL,'p'
	JZ PAUSE
	CMP AL,'P'
	JZ PAUSE
	
	XOR AH,AH                       ;暂停恢复
	CMP AL,'s'
	JZ START_K
	CMP AL,'S'
	JZ START_K
	
	XOR AH,AH                      ;切上一首
	CMP AL,'<'
	JZ PSONG
	
	XOR AH,AH                      ;切下一首
	CMP AL,'>'
	JZ NSONG
	
	JMP ERROR_W
START_K:                           ;暂停后开始
	LEA BX,EXIT_BOX
	MOV BYTE PTR [BX],0
	PUSH SI
	LEA SI,S
	CALL DISPLAY
	POP SI
	JMP OUT_K
PAUSE:                             ;暂停
	LEA BX,EXIT_BOX
	MOV BYTE PTR [BX],1	
	PUSH SI
	LEA SI,P
	CALL DISPLAY
	POP SI
	JMP OUT_K
	
PSONG:
	LEA BX,EXIT_BOX	
	PUSH SI
	LEA SI,Q
	CMP SITE,31H
	JZ  PS1
	DEC SITE
	
PS2:
	MOV BYTE PTR [BX],4
	POP SI
	JMP OUT_K
	
PS1:
	MOV SITE,'5'
	JMP PS2
	
NSONG:
	LEA BX,EXIT_BOX	
	PUSH SI
	LEA SI,Q
	CMP SITE,35H
	JZ  NS1
	INC SITE
NS2:
	MOV BYTE PTR [BX],4
	POP SI
	JMP OUT_K
NS1:
	MOV SITE,'1'
	JMP NS2 		
QUIT:                              ;退出程序
	LEA BX,EXIT_BOX	
	MOV BYTE PTR [BX],2
	PUSH SI
	LEA SI,Q
	CALL DISPLAY
	POP SI
	JMP OUT_K	
DOWN:                              ;减速	
	LEA BX,BEAT
	MOV AX,[BX]
	ADD AX,1000
	MOV [BX],AX
	PUSH SI
	LEA SI,D
	CALL DISPLAY
	POP SI
	JMP OUT_K	
UP:                                 ;加速
	LEA BX,BEAT
	MOV AX,[BX]
	SUB AX,1000
	MOV [BX],AX
	PUSH SI
	LEA SI,U
	CALL DISPLAY
	POP SI
	JMP OUT_K
ERROR_W:                            ;程序出错
	LEA BX,EXIT_BOX	
	MOV BYTE PTR [BX],3
	JMP OUT_K
			
OUT_K:                              
	POP AX                          ;恢复AX的值
	RET
KEY_C ENDP

;**************开蜂鸣器子函数***************
SPK_ON PROC NEAR
	PUSH AX			;保存ax的值	
	IN AL,61H		;获取61h端口的当前值
	OR AL,03H		;把61h端口低二位置1,即打开蜂鸣器
	OUT 61H,AL		;输出数据到61h端口
	POP AX			;恢复ax的值
	RET
SPK_ON  ENDP

;**************关蜂鸣器子函数***************
SPK_OFF PROC NEAR
	PUSH AX			;保存ax的值
	IN AL,61H		;获取61h端口的当前值	
	AND AL,0FCH		;把61h端口低二位置0,即关闭蜂鸣器
	OUT	61H,AL		;输出数据到61h端口
	POP AX			;恢复ax的值
	RET
SPK_OFF ENDP

CODES ENDS
  END MAIN





