      *PROJETO CAIXA ELETRONICO
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXA-ELETRONICO.

       DATA DIVISION.

           WORKING-STORAGE SECTION.
           77 WS-OPCAO PIC X(2).
           77 WS-SALDO PIC 9(10)V99 VALUE ZERO.
           77 WS-DEPOSITO PIC 9(10)V99 VALUE ZERO.
           77 WS-SAQUE PIC 9(10)V99 VALUE ZERO.
           77 WS-OPCAO-SUBMENU PIC x(2).

       PROCEDURE DIVISION.
           P001-MENU1.
               DISPLAY "--------------------------".
               DISPLAY "        BANCO MGSL        ".
               DISPLAY "--------------------------".
               DISPLAY "1 - DEPOSITO".
               DISPLAY "2 - SAQUE".
               DISPLAY "3 - CONSULTAR SALDO".
               DISPLAY "4 - SAIR".
               ACCEPT WS-OPCAO.

           EVALUATE WS-OPCAO
               WHEN '1'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "DIGITE O VALOR DO DEPOSITO"
                   ACCEPT WS-DEPOSITO
                   COMPUTE WS-SALDO = WS-SALDO + WS-DEPOSITO
                   DISPLAY "DEPOSITO REALIZADO COM SUCESSO"
                   PERFORM P001-MENU2

               WHEN '2'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY " DIGITE O VALOR DO SAQUE  "
                   ACCEPT WS-SAQUE
                   IF WS-SAQUE > WS-SALDO
                   THEN
                       DISPLAY "    SALDO INSUFICIENTE    "
                       PERFORM P001-MENU1
                   ELSE
                       COMPUTE WS-SALDO = WS-SALDO - WS-SAQUE
                       DISPLAY "      SAQUE EFETIVADO     "
                       PERFORM P001-MENU1
                   END-IF
                   PERFORM P001-MENU2
               WHEN '3'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY " O SALDO DISPONIVEL E DE: "
                   DISPLAY "R$:   " WS-SALDO
                   PERFORM P001-MENU2
               WHEN '4'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "  OBRIGADO, VOLTE SEMPRE! "
                   STOP RUN
               WHEN OTHER
                   PERFORM P001-MENU1
           END-EVALUATE.

           P001-MENU2.
               DISPLAY "---------------------------"
               DISPLAY "        BANCO MGSL         "
               DISPLAY "---------------------------"
               DISPLAY "1 - VOLTAR AO MENU ANTERIOR"
               DISPLAY "2 - SAIR"
               ACCEPT WS-OPCAO-SUBMENU.

           EVALUATE WS-OPCAO-SUBMENU
               WHEN '1'
                   PERFORM P001-MENU1
               WHEN '2'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "  OBRIGADO, VOLTE SEMPRE! "
                   STOP RUN
           END-EVALUATE.

           END PROGRAM CAIXA-ELETRONICO.
