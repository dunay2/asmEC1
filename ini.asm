section .data               
;Cambiar Nombre y Apellido por vuestros datos.
developer db "_DIEGO_ _RIOS_",0

;Constantes que también están definidas en C.
DimMatrix    equ 4      
SizeMatrix   equ 16

section .text            
;Variables definidas en Ensamblador.
global developer                        

;Subrutinas de ensamblador que se llaman des de C.
global showNumberP1, updateBoardP1, calcIndexP1, rotateMatrixRP1, copyMatrixP1
global shiftNumbersRP1, addPairsRP1
global readKeyP1, playP1

;Variables definidas en C.
extern rowScreen, colScreen, charac
extern m, mRotated, number, score, state

;Funciones de C que se llaman desde ensamblador
extern clearScreen_C, printBoardP1_C, gotoxyP1_C, getchP1_C, printchP1_C
extern insertTileP1_C, printMessageP1_C 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATENCIÓN: Recordad que en ensamblador las variables y los parámetros 
;;   de tipo 'char' se tienen que asignar a registros de tipo
;;   BYTE (1 byte): al, ah, bl, bh, cl, ch, dl, dh, sil, dil, ..., r15b
;;   las de tipo 'short' se tiene que assignar a registros de tipo 
;;   WORD (2 bytes): ax, bx, cx, dx, si, di, ...., r15w
;;   las de tipo 'int' se tiene que assignar a registros de tipo  
;;   DWORD (4 bytes): eax, ebx, ecx, edx, esi, edi, ...., r15d
;;   las de tipo 'long' se tiene que assignar a registros de tipo 
;;   QWORD (8 bytes): rax, rbx, rcx, rdx, rsi, rdi, ...., r15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Las subrutinas en ensamblador que tenéis que implementar son:
;;   showNumberP1, updateBoardP1, rotateMatrixRP1, 
;;   copyMatrixP1, shiftNumbersRP1, addPairsRP1.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Esta subrutina se da hecha. NO LA PODÉIS MODIFICAR.
; Situar el cursor en una fila indicada por la variable (rowScreen) y en 
; una columna indicada por la variable (colScreen) de pantalla 
; llamando a la función gotoxyP1_C.
; 
; Variables globales utilizadas:	
; rowScreen: Fila de la pantalla donde posicionamos el cursor.
; colScreen: Columna de la pantalla donde posicionamos el cursor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gotoxyP1:
   push rbp
   mov  rbp, rsp
   ;guardamos el estado de los registros del procesador porque
   ;las funciones de C no mantienen el estado de los registros.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   call gotoxyP1_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax

   mov rsp, rbp
   pop rbp
   ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Esta subrutina se da hecha. NO LA PODÉIS MODIFICAR.
; Mostrar un carácter guradado en la varaile (charac) en pantalla, en
; la posición donde está el cursor llamando a la función printchP1_C.
; 
; Variables globales utilizadas:	
; charac   : Carácter que queremos mostrar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printchP1:
   push rbp
   mov  rbp, rsp
   ;guardamos el estado de los registros del procesador porque
   ;las funciones de C no mantienen el estado de los registros.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15

   call printchP1_C
 
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax

   mov rsp, rbp
   pop rbp
   ret
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Esta subrutina se da hecha. NO LA PODÉIS MODIFICAR.
; Leer una tecla y guardar el carácter asociado en la varaible (charac) 
; sin mostrarlo en pantalla, llamando a la función getchP1_C
; 
; Variables globales utilizadas:	
; charac   : Carácter que queremos mostrar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
getchP1:
   push rbp
   mov  rbp, rsp
   ;guardamos el estado de los registros del procesador porque
   ;las funciones de C no mantienen el estado de los registros.
   push rax
   push rbx
   push rcx
   push rdx
   push rsi
   push rdi
   push r8
   push r9
   push r10
   push r11
   push r12
   push r13
   push r14
   push r15
   push rbp

   call getchP1_C
 
   pop rbp
   pop r15
   pop r14
   pop r13
   pop r12
   pop r11
   pop r10
   pop r9
   pop r8
   pop rdi
   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   
   mov rsp, rbp
   pop rbp
   ret 
   

;;;;;
; Convierte el número de la variable (number) de tipo int (DWORD) de 6
; dígitos (number <= 999999) a caracteres ASCII que representen su valor.
; Si (number) es más grande de 999999 cambiaremos el valor a 999999.
; Se tiene que dividir el valor entre 10, de forma iterativa, hasta 
; obtener los 6 dígitos. 
; A cada iteración, el residuo de la división que es un valor
; entre (0-9) indica el valor del dígito que tenemos que convertir
; a ASCII ('0' - '9') sumando '0' (48 decimal) para poderlo mostrar.
; Cuando el cociente sea 0 mostraremos espacios en la parte no significativa.
; Por ejemplo, si number=103 mostraremos "   103" y no "000103".
; Se tienen que mostrar los dígitos (carácter ASCII) desde la posición 
; indicada por las variables (rowScreen) y (colScreen), posición de les 
; unidades, hacia la izquierda.
; Como el primer dígito que obtenemos son las unidades, después las decenas,
; ..., para mostrar el valor se tiene que desplazar el cursor una posición
; a la izquierda en cada iteración.
; Para posicionar el cursor se llamada a la función gotoxyP1 y para 
; mostrar los caracteres a la función printchP1.
;
; Variables globales utilizadas:    
; number    : Valor que queremos mostrar.
; rowScreen : Fila para posicionar el cursor a la pantalla.
; colScreen : Columna para posicionar el cursor a la pantalla.
; charac    : Carácter que queremos mostrar
;;;;;
showNumberP1:
   push rbp
   mov  rbp, rsp
   
    push ax ;n
   push rbx ;divisor
   push rdx; resto
   push si; i
   
   ;inicializacion
   mov eax, DWORD[number];n=number
   mov sil,0

   cmp eax,999999
   jle bucle; if (n > 999999) n = 999999
   mov eax, 999999; eax valor n                             
   
   bucle: cmp sil, 6 ; hasta i=5
      je fin_subrutina; acaba si es 6   
      mov BYTE[charac],' '; limpiar
      cmp eax ,0; elude bloque if si n es <=0 

      jle fin_if ;if (n>0)   
         ;dividendo es 32 bits fuente es de 16 bits: AX = DX:AX / fuente, DX = DX:AX mod fuente
         mov edx, 0; limpiar
         mov ebx,10 ; asigna divisor
         ; EAX / EBX
         div ebx 
         ;el cociente queda en EAX
         ;el resto en el primer bit de DX(dl)
         ; Sumar 0 al residuo (Convertir a ASCII)  
         add dl,'0'
         mov BYTE[charac], dl
      fin_if:
   
      call gotoxyP1    
      call printchP1
      dec DWORD[colScreen];colScreen--;
      inc sil;i++
   jmp bucle

   fin_subrutina:

   pop si
   pop rdx
   pop bx
   pop ax
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;
; Actualizar el contenido del Tablero de Juego con los datos de 
; la matriz (m) y los puntos del marcador (score) que se han hecho.  
; Se tiene que recorrer toda la matriz (m), y para cada elemento de 
; la matriz posicionar el cursor en pantalla y mostrar el número de 
; esa posición de la matriz.
; Para recorrer la matriz en assemblador el indice va de 0 (posició [0][0])
; a 30 (posición [3][3]) con incrementos de 2 porque los datos son de 
; tipo short(WORD) 2 bytes.
; Después, mostrar el marcador (score) en la parte inferior del tablero
; fila 18, columna 26 llamando a la subrutina showNumberP1.
; Finalmente posicionar el cursor en la fila 18, columna 28 llamando a
; la subrutina goroxyP1.
;
; Variables globales utilizadas:    
; rowScreen : Fila para posicionar el cursor en pantalla.
; colScreen : Columna per a posicionar el cursor en pantalla.
; m         : Matriz 4x4 donde hay los números del tablero de juego.
; score     : Puntos acumulados en el marcador hasta el momento.
; number    : Número que queremos mostrar.
;;;;;  
updateBoardP1:
   push rbp
   mov  rbp, rsp
   
   
  ;************
   push rax; dividendo/cociente eax
   push rbx; divisor ebx
   push rcx; rowScreenAux ecx 
   push rdx; resto 
   push rsi; contador 
   push rdi; gestor matriz
   push r8; colScreenAux 

   mov eax,0;
   mov ecx, 8 ;rowScreenAux = 10;  
   mov esi,0;
   mov rdi,0

   extLoop:

   mov r8d,17 ; colScreenAux = 17;
   add ecx,2 ;  rowScreenAux+=2
   cmp sil,SizeMatrix; si ha recorrido la matriz, acaba
   je end_fillMatrix

   fillMatrix:            
      
      mov di, WORD[m+esi*2]; 
      mov DWORD[number], edi ; number = m[i][j];      
         
      mov DWORD[rowScreen],ecx ;rowScreen = rowScreenAux;
      mov DWORD[colScreen],r8d; colScreen = colScreenAux;       

      call showNumberP1;       
      add r8d,9; colScreenAux = colScreenAux + 9;                  
      inc sil;  i++

      ;bucle exterior mod DimMatrix check de salto
      mov eax,esi ;se prepara el dividendo      
      mov edx, 0; limpiar el resto
      mov ebx,DimMatrix ; asigna divisor      
      div ebx ; EAX / EBX
      cmp dl,0 ;el resto en el primer bit de DX(dl)el cociente queda en EAX   
      jg fillMatrix
      jmp extLoop 
      
   end_fillMatrix:
      
   mov eax,DWORD[score];   number = score;
   mov DWORD[number],eax
   mov DWORD[rowScreen],18; rowScreen = 18;
   mov DWORD[colScreen],26; colScreen = 26;
   call showNumberP1;   
   mov DWORD[rowScreen],18; rowScreen = 18;
   mov DWORD[colScreen],26; colScreen = 26;
   call gotoxyP1;

   pop r8;
   pop rdi; gestor matriz
   pop rsi; contador 
   pop rdx; resto 
   pop rcx; rowScreenAux ecx 
   pop rbx; colScreenAux ebx
   pop rax; cociente eax
   ;****
   mov rsp, rbp
   pop rbp
   ret


;;;;;      
; Rotar a la derecha la matriz (m), sobre la matriz (mRotated). 
; La primera fila pasa a ser la cuarta columna, la segunda fila pasa 
; a ser la tercera columna, la tercera fila pasa a ser la segunda
; columna y la cuarta fila pasa a ser la primer columna.
; En el enunciado se explica con más detalle como hacer la rotación.
; NOTA: NO es lo mismo que hacer la matriz traspuesta.
; La matriz (m) no se tiene que modificar, 
; los cambios se tiene que hacer en la matriz (mRotated).
; Para recorrer la matriz en ensamblador el indice va de 0 (posición [0][0])
; a 30 (posición [3][3]) con incrementos de 2 porque los datos son de 
; tipo short(WORD) 2 bytes.
; Para acceder a una posición concreta de la matriz desde ensamblador 
; hay que tener en cuenta que el índice es:(index=(fila*DimMatrix+columna)*2),
; multiplicamos por 2 porque los datos son de tipo short(WORD) 2 bytes.
; Una vez se ha hecho la rotación, copiar la matriz (mRotated) a la 
; matriz (m) llamando a la subrtuina copyMatrixP1.
;
; Variables globales utilizadas:    
; m        : matriz 4x4 donde hay los números del tablero de juego.
; mRotated : Matriz 4x4 para hacer la rotación.
;;;;;  
rotateMatrixRP1:
   push rbp
   mov  rbp, rsp
   
   push rax ; contador 0 hasta 15
push rbx ; contador j  3 hasta 0 para calculo de transposicion
push rcx ; contador k posicion ciclo 0-3 
push rdx ; contenido aux
push rsi; calculo direccion

   mov eax, 0
   mov rbx, DimMatrix
   mov rsi,0

   reset_k:
   mov cx,0
   dec bx ; se comienza en 3
   blucle_k:

   ;copiar matriz
   mov dx, WORD[m+eax*2] ; copiar contenido  en dx
   mov  si,cx ;  calcuar desplazamiento
   imul si,DimMatrix; 
   add  si, bx ; 
   mov [mRotated +esi*2],dx; copiar contenido a posicion matriz  
   
   inc ax
   inc cx
   cmp eax,SizeMatrix
   je fin
   cmp cx,DimMatrix
   je reset_k 
   jmp blucle_k
   fin:  

   call copyMatrixP1

   pop rsi
   pop rdx
   pop rcx
   pop rbx
   pop rax
   ;***
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Copiar los valores de la matriz (mRotated) a la matriz (m).
; La matriz (mRotated) no se tiene que modificar, 
; los cambios se tienen que hacer en la matriz (m).
; Para recorrer la matriz en ensamblador el índice va de 0 (posición [0][0])
; a 30 (posición [3][3]) con incrementos de 2 porque los datos son de 
; tipo short(WORD) 2 bytes.
; No se muestra la matriz. 
;
; Variables globales utilizadas:    
; m        : Matriz 4x4 donde hay los números del tablero de juego.
; mRotated : Matriz 4x4 para hacer la rotación.
;;;;;  
copyMatrixP1:
   push rbp
   mov  rbp, rsp
   
   ;***************
 push rax; aux
 push rbx; indice matriz

   mov ebx, 0 
   copiar:
   mov ax, WORD[mRotated+ebx*2]     
   mov WORD[m+ebx*2],ax 
   inc bx
   cmp bx, SizeMatrix
   jl copiar

   pop rbx
   pop rax
;*************
   
   
   mov rsp, rbp
   pop rbp
   ret


;;;;;  
; Desplazar a la derecha los números de cada fila de la matriz (m), 
; manteniendo el orden de los números y poniendo los ceros a la izquierdaa.
; Recorrer la matriz por filas de derecha a izquierda y de abajo hacia arriba.  
; Si se desplaza un número (NO LOS CEROS) pondremos la variable 
; (state) a '2'.
; Si una fila de la matriz es: [0,2,0,4] y state = '1', quedará [0,0,2,4] 
; y state= '2'.
; En cada fila, si encuentra un 0, mira si hay un número distinto de zero,
; en la misma fila para ponerlo en aquella posición.
; Para recorrer la matriz en ensamblador, en este caso, el índice va de la
; posición 30 (posición [3][3]) a la 0 (posición [0][0]) con decrementos de
; 2 porque los datos son de tipo short(WORD) 2 bytes.
; Per a acceder a una posición concreta de la matriz desde ensamblador 
; hay que tener en cuenta que el índice es:(index=(fila*DimMatrix+columna)*2),
; multiplicamos por 2 porque los datos son de tipo short(WORD) 2 bytes.
; Los cambios se tienen que hacer sobre la misma  matriz.
; No se tiene que mostrar la matriz.
;
; Variables globales utilizadas:    
; state    : Estado del juego. (2: Se han hecho movimientos).
; m        : Matriz 4x4 donde hay los números del tablero de juego.
;;;;;  
shiftNumbersRP1:
   push rbp
   mov  rbp, rsp

;***
push rax ; i
push rbx ; j 
push rcx ; k
push rsi ;aux
push rdi; aux2
push rdx ; valor matriz

;inicializacion 
    mov rax, DimMatrix
    dec rax ; se usa DimMatrix-1
    mov rbx, rax
    mov rcx,0
    mov rsi,0
    mov rdi,0
    mov rdx,0

out_iter:
    inner_iter:
        ;if (m[i][j] == 0)
        ;ubicar la posicion en la matriz columna+4*fila ;2(4x+y)
        mov si,ax    ;<-- 3
        imul si,4    ;<-- 3x4=12
        add si,bx    ;<-- 12 +3=15
        mov dx,word[m+esi*2]
        cmp word[m+esi*2],0 ;
        
        jne endif_ij_not_0; jump if (m[i][j] != 0) 
            mov cx,bx      
            dec cx ;k = j-1;
            k_while:
                cmp cx,0      ;while (k>=0 && m[i][k]==0) k--;
                jl end_k_while  ;1. k<0 salta        
                ;ubicar la posicion en la matriz columna+4*fila
                mov si,ax   ; guardamos la fila
                imul si,4     ;la multiplicamos x 4
                add si,cx  
                cmp word[m+esi*2],0  ;2. m[i][k]==0)

                jne end_k_while ; k>=0 pero m[i][k]!=0
                dec cx        ;k--
                jmp k_while
            end_k_while:
            ;if (k==-1)
            cmp cx,-1  
            jne else_k_eq
                mov bx,0 ;  j=0
                jmp endif_ij_not_0
            else_k_eq: ;m[i][j]=m[i][k];
                ;se calcula m[i][k] 
                mov si,ax   ; guardamos la fila
                imul si,4     ;la multiplicamos x 4
                add si,cx  
                mov di,word[m+esi*2]   ; se guarda el valor de m[i][k] en di
                      ; se calcula mov m[i][j]  
                mov si,ax   ; guardamos la fila
                imul si,4   ;la multiplicamos x 4
                add si,bx  
                mov word[m+esi*2],di ; m[i][j]=di 

                ;m[i][k]= 0; 
                mov si,ax   ; guardamos la fila
                imul si,4     ;la multiplicamos x 4
                add si,cx  
                mov word[m+esi*2],0  ;2. m[i][k]==0)
                mov BYTE[state],'2' ;state='2';
        endif_ij_not_0:

        dec bx
        cmp bx,0
        jg inner_iter
    fin_inner_iter:
    mov bx,DimMatrix
    dec bx
    dec ax
    cmp ax,0
    jge out_iter
fin_out_iter:
pop rdx
pop rdi
pop rsi 
pop rcx
pop rbx
pop rax
    ;********** 
   
   
   mov rsp, rbp
   pop rbp
   ret
      

;;;;;  
; Emparejar números iguales desde la derecha de la matriz (m) y acumular 
; los puntos en el marcador sumando los puntos de las parejas que se hagan.
; Recorrer la matriz por filas de dercha a izquierda y de abajo hacia arriba. 
; Cuando se encuentre una pareja, dos casillas consecutivas con el mismo 
; número, juntamos la pareja poniendo la suma de los números de la 
; pareja en la casilla de la derechay un 0 en la casilla de la izquierda y 
; acumularemos esta suma (puntos que se ganan).
; Si una fila de la matriz es: [8,4,4,2] y state = 1, quedará [8,0,8,2], 
; p = p + (4+4) y state = '2'.
; Si al final se ha juntado alguna pareja (puntos>0), pondremos la variable 
; (state) a '2' para indicar que se ha movido algún número y actualizaremos
; la variable (score) con los puntos obtenidos de hacer las parejas.
; Para recorrer la matriz en ensamblador, en este caso, el índice va de la
; posición 30 (posición [3][3]) a la 0 (posición [0][0]) con decrementos de
; 2 porque los datos son de tipo short(WORD) 2 bytes.
; Para acceder a una posición concreta de la matriz desde ensamblador 
; hay que tener en cuenta que el índice es:(index=(fila*DimMatrix+columna)*2),
; multiplicamos por 2 porque los datos son de tipo short(WORD) 2 bytes.
; Los cambios se tienen que hacer sobre la misma  matriz.
; No se tiene que mostrar la matriz.
; 
; Variables globales utilizadas:    
; m        : Matriz 4x4 donde hay los números del tablero de juego.
; score    : Puntos acumulados hasta el momento.
; state    : Estado del juego. (2: Se han hecho movimientos).
;;;;;  
addPairsRP1:
   push rbp
   mov  rbp, rsp

          ;***
   push rax ; i
   push rbx ; j 
   push rcx ; p
   push rsi ;aux
   push rdi; aux2

   ;inicializacion 
   mov rax, DimMatrix
   dec rax ; se usa DimMatrix-1
   mov rbx, rax
   mov ecx,0
   mov rsi,0
   mov rdi,0; 

   out_iterPairs:
      inner_iterPairs:
            ;enunciar condicion 1 if (m[i][j] != 0)        
            mov si,ax    ;ubicar la posicion en la matriz columna+4*fila ;2(4x+y)
            imul si,DimMatrix    
            add si,bx    
            mov di,word[m+esi*2] ; m[i][j]
            cmp di,0 ; (m[i][j] <--> 0)
            ;enunciar condicion 1
            je eif_ij_not_0; jump if not (m[i][j] != 0) 
            ;enunciar condicion 2   (m[i][j]==m[i][j-1]
            mov si,ax    ;ubicar la posicion en la matriz columna+4*fila ;2(4x+y)
            imul si,DimMatrix    
            add si,bx    
            dec si  ; la comparación es con el elemento anterior
            cmp di,word[m+esi*2]
            jne eif_ij_not_0 ; si no se cumple la condicion 2 saltar  
                     
                  imul di,2      
                  mov si,ax    ;ubicar la posicion en la matriz columna+4*fila ;2(4x+y)
                  imul si,DimMatrix    
                  add si,bx    
                  mov word[m+esi*2],di   ; asignar m[i][j]  = m[i][j]*2;
               
                  mov si,ax    ;ubicar la posicion en la matriz columna+4*fila ;2(4x+y)
                  imul si,DimMatrix    
                  add si,bx    
                  dec si  ; asignacion sobre elemento anterior
                  mov word[m+esi*2],0   ; m[i][j-1]= 0;

                  add ecx,edi; p = p + m[i][j];
               
         eif_ij_not_0:

         dec bx
         cmp bx,0
         jg inner_iterPairs
      fin_inner_iterPairs:
      mov bx,DimMatrix
      dec bx
      dec ax
      cmp ax,0
      jge out_iterPairs
   fin_out_iterPairs:

   cmp ecx,0 ; compare p
   jle end; if (p > 0) {
      mov BYTE[state],'2';   state = '2';
      add dword[score],ecx ;score = score + p;
         
   end:

   pop rdi
   pop rsi 
   pop rcx
   pop rbx
   pop rax 
    ;**********  
   
   mov rsp, rbp
   pop rbp
   ret
   

;;;;;; 
; Esta subrutina se da hecha. NO LA PODÉIS MODIFICAR.
; Leer una tecla (una sola vez, sin hacer un bucle) llamando a la 
; subrutina getchP1 que la guarda en la variable (charac).
; Según la tecla leída llamaremos a las subrutinas que corresponda.
;    ['i' (arriba),'j'(izquierda),'k' (a bajo) o 'l'(derecha)] 
; Desplazar los números y hacer las parejas según la dirección escogida.
; Según la tecla pulsada, rotar la matriz llamando (rotateMatrixRP1),
; para poder hacer los desplazamientos de los números hacia la derecha
; (shiftNumbersRP1),  hacer las parejas hacia la derecha (addPairsRP1) 
; y volver a desplazar los números hacia la izquierda (shiftNumbersRP1) 
; con las parejas hechas, después seguir rotando llamando (rotateMatrixRP1) 
; hasta dejar la matriz en la posición inicial. 
; Para la tecla 'l' (dercha) no hay que hacer rotaciones, para el
; resto se tienen que hacer 4 rotaciones.
;    '<ESC>' (ASCII 27)  poner (state = '0') para salir del juego.
; Si no es ninguna de estas teclea no hacer nada.
; Los cambios producidos por estas subrutina no se tiene que mostrar en 
; pantalla, por lo tanto, hay que actualizar después el tablero llamando 
; la subrutina UpdateBoardP1.
;
; Variables globales utilizadas: 
; charac   : Carácter que leemos de teclado.
; state    : Indica el estado del juego. '0':salir, '1':jugar
;;;;;  
readKeyP1:
   push rbp
   mov  rbp, rsp

   push rax 
      
   call getchP1    ; Leer una tecla y dejarla en charac.
   mov  al, BYTE[charac]
      
   readKeyP1_i:
   cmp al, 'i'      ; arriba
   jne  readKeyP1_j
      call rotateMatrixRP1
      
      call shiftNumbersRP1
      call addPairsRP1
      call shiftNumbersRP1  
      
      call rotateMatrixRP1
      call rotateMatrixRP1
      call rotateMatrixRP1
      jmp  readKeyP1_End
      
   readKeyP1_j:
   cmp al, 'j'      ; izquierda
   jne  readKeyP1_k
      call rotateMatrixRP1
      call rotateMatrixRP1
      
      call shiftNumbersRP1
      call addPairsRP1
      call shiftNumbersRP1  
      
      call rotateMatrixRP1
      call rotateMatrixRP1
      jmp  readKeyP1_End
      
   readKeyP1_k:
   cmp al, 'k'      ; abajo
   jne  readKeyP1_l
      call rotateMatrixRP1
      call rotateMatrixRP1
      call rotateMatrixRP1
      
      call shiftNumbersRP1
      call addPairsRP1
      call shiftNumbersRP1  
      
      call rotateMatrixRP1
      jmp  readKeyP1_End

   readKeyP1_l:
   cmp al, 'l'      ; derecha
   jne  readKeyP1_ESC
      call shiftNumbersRP1
      call addPairsRP1
      call shiftNumbersRP1  
      jmp  readKeyP1_End

   readKeyP1_ESC:
   cmp al, 27      ; Salir del programa
   jne  readKeyP1_End
      mov BYTE[state], '0'

   readKeyP1_End:
   pop rax
      
   mov rsp, rbp
   pop rbp
   ret


;;;;;
; Juego del 2048
; Función principal del juego
; Permite jugar al juego del 2048 llamando todas las funcionalidades.
; Esta subrutina se da hecha. NO LA PODÉIS MODIFICAR.
;
; Pseudo-código:
; Inicializar estado del juego, (state='1')
; Borrar pantalla (llamar a la función clearScreen_C).
; Mostrar el tablero de juego (llamar a la función PrintBoardP1_C).
; Actualizar el contenido del Tablero de Juego y los puntos que se han 
; hecho (llamar a la función updateBoardP1).
; Mientras (state=='1') hacer
;   Leer una tecla (llamar a la función readKeyP1). Según la tecla 
;     leída llamar a las funciones que corresponda.
;     - ['i','j','k' o 'l'] desplazar los números y hacer las parejas 
;                           según la dirección escogida.
;     - '<ESC>'  (código ASCII 27) poner (state = '0') para salir.   
;   Si hemos movido algún número al hacer los desplazamientos o al hacer
;   las parejas (state==2) generar una nueva ficha (llamando a la función
;   insertTileP1_C) y poner la variable state a '1' (state='1').
;   Actualizar el contenido del Taublero de Juego y los puntos que se han
;   hecho (llamar a la función updateBoardP1).
; Fin mientras.
; Mostrar un mensaje debajo del tablero según el valor de la variable 
; (state). (llamar a la función printMessageP1_C).
; Salir: 
; Se ha terminado el juego.

; Variables globales utilizadas: 
; state  : Indica el estado del juego. '0':salir, '1':jugar
;;;;;  
playP1:
   push rbp
   mov  rbp, rsp
   
   mov BYTE[state], '1'      ;state = '1';  //estado para empezar a jugar   
   
   call clearScreen_C
   call printBoardP1_C
   call updateBoardP1

   playP1_Loop:               ;while  {     //Bucle principal.
   cmp  BYTE[state], '1'     ;(state == '1')
   jne  playP1_End
      
      call readKeyP1          ;readKeyP1_C();
      cmp BYTE[state], '2'   ;state == '2' //Si se ha hecho algun movimiento
      jne playP1_Next 
         call insertTileP1_C   ;insertTileP1_C(); //Añadr ficha (2)
         mov BYTE[state],'1'  ;state = '1';
      playP1_Next
      call updateBoardP1       ;updateBoardP1_C();
      
   jmp playP1_Loop

   playP1_End:
   call printMessageP1_C      ;printMessageP1_C();
                              ;Mostrar el mensaje para indicar como acaba.
   mov rsp, rbp
   pop rbp
   ret
