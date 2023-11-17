      *PROJETO CAIXA ELETRONICO
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXA-ELETRONICO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.

           WORKING-STORAGE SECTION.

      * GLOBAL
           77 WS-OPCAO PIC X(2).

      *L001 - ACESSO / CRIACAO
           77 WS-LOGIN PIC X(20).
           77 WS-SENHA PIC 9(06).
           77 WS-NOVO-LOGIN PIC X(12).
           77 WS-NOVA-SENHA PIC 9(06).
           77 WS-SENHA-VALIDACAO PIC 9(06).

      *P001 - MENU LOGADO
           77 WS-SALDO PIC 9(10) value ZEROS.
           77 WS-DEPOSITO PIC 9(10)V99 VALUE ZERO.
           77 WS-SAQUE PIC 9(10)V99 VALUE ZERO.
           77 WS-OPCAO-SUBMENU PIC x(2).

       PROCEDURE DIVISION.
       PERFORM L003-INICIO.

      * MENU DE CADASTRO
           L001-CADASTRO.
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "    INFORME SEU LOGIN:    "
                   ACCEPT WS-NOVO-LOGIN.
                   DISPLAY "--------------------------"
                   DISPLAY "     INFORME UMA SENHA    "
                   DISPLAY "     DE ATE 6 DIGITOS:    "
                   ACCEPT WS-NOVA-SENHA.
                   DISPLAY "--------------------------"
                   DISPLAY "   CONFIRME SUA SENHA:    "
                   ACCEPT WS-SENHA-VALIDACAO.
                   DISPLAY "--------------------------"
                   DISPLAY "ACESSO CRIADO COM SUCESSO "
                   PERFORM L003-INICIO.
      * MENU DE LOGIN
           L002-LOGIN.
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "    INFORME SEU LOGIN:    "
                   ACCEPT WS-LOGIN.
                   DISPLAY "--------------------------"
                   DISPLAY "    INFORME SUA SENHA:    "
                   ACCEPT WS-SENHA.
                   PERFORM P001-MENU.

           L003-INICIO.
                   DISPLAY "     SEJA BEM VINDO AO    "
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "--------------------------"
                   DISPLAY "1 - ACESSAR SUA CONTA     "
                   DISPLAY "2 - CRIAR UMA CONTA NOVA  "
                   DISPLAY "3 - SAIR                  "
                   ACCEPT WS-OPCAO.

           EVALUATE WS-OPCAO
               WHEN '1'
                   PERFORM L002-LOGIN
               WHEN '2'
                   PERFORM L001-CADASTRO
               WHEN '3'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "  OBRIGADO, VOLTE SEMPRE! "
                   STOP RUN
               WHEN OTHER
                   DISPLAY "ESCOLHA UMAS DAS OPCOES"
                   PERFORM L003-INICIO
           END-EVALUATE.

      *MENU LOGADO
           P001-MENU.
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
                   PERFORM P002-MENU
               WHEN '2'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY " DIGITE O VALOR DO SAQUE  "
                   ACCEPT WS-SAQUE
                   IF WS-SAQUE > WS-SALDO
                   THEN
                       DISPLAY "    SALDO INSUFICIENTE    "
                       PERFORM P002-MENU
                   ELSE
                       COMPUTE WS-SALDO = WS-SALDO - WS-SAQUE
                       DISPLAY "      SAQUE EFETIVADO     "
                       PERFORM P002-MENU
                   END-IF
               WHEN '3'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY " O SALDO DISPONIVEL E DE: "
                   DISPLAY "R$:   " WS-SALDO
                   PERFORM P002-MENU
               WHEN '4'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "  OBRIGADO, VOLTE SEMPRE! "
                   STOP RUN
               WHEN OTHER
                   DISPLAY "------------------------------"
                   DISPLAY "          BANCO MGSL          "
                   DISPLAY "------------------------------"
                   DISPLAY " A OPCAO ESCOLHIDA E INVALIDA "
                   PERFORM P001-MENU
           END-EVALUATE.

           P002-MENU.
               DISPLAY "---------------------------"
               DISPLAY "        BANCO MGSL         "
               DISPLAY "---------------------------"
               DISPLAY "1 - VOLTAR AO MENU ANTERIOR"
               DISPLAY "2 - SAIR"
               ACCEPT WS-OPCAO-SUBMENU.

           EVALUATE WS-OPCAO-SUBMENU
               WHEN '1'
                   PERFORM P001-MENU
               WHEN '2'
                   DISPLAY "--------------------------"
                   DISPLAY "        BANCO MGSL        "
                   DISPLAY "--------------------------"
                   DISPLAY "  OBRIGADO, VOLTE SEMPRE! "
                   STOP RUN
           END-EVALUATE.

           END PROGRAM CAIXA-ELETRONICO.
