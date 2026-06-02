       IDENTIFICATION DIVISION.
        PROGRAM-ID. SISTEMA-BANCARIO.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
              FILE-CONTROL.
                SELECT CLIENTES ASSIGN TO 'dados_clientes_cobol.txt'
                    ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
        FILE SECTION.
        FD  CLIENTES.
        01  CLIENTE.
           05  ID-CLIENTE          PIC X(05).
           05  CPF                 PIC X(11).
           05  NOME                PIC X(15).
           05  SOBRENOME           PIC X(20).
           05  SENHA               PIC X(08).
           05  SALDO        PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  EM-EMPRESTIMO       PIC X(01).
           05  LIMITE-EMPRESTIMO   PIC 9(13)V99.
           05  MOVIMENTACOES       PIC 9(04).

        WORKING-STORAGE SECTION.
        01  WS-CPF                 PIC X(11).
        01  WS-NOME                PIC X(15).
        01  WS-SOBRENOME           PIC X(20).
        01  WS-SENHA               PIC X(08).
        01  WS-SALDO               PIC S9(13)V99 SIGN LEADING SEPARATE.
        01  WS-EM-EMPRESTIMO       PIC X(01).
        01  WS-LIMITE-EMPRESTIMO   PIC 9(13)V99.
        01  WS-MOVIMENTACOES       PIC 9(04).
        01  LEITURA-FINALIZADA     PIC X(01) VALUE 'N'.
        01  OPCAO                  PIC 9(01).
        01  CLIENTE-ENCONTRADO     PIC X(01) VALUE 'N'.



       PROCEDURE DIVISION.
            PERFORM AVALIAR-ACESSO.
            PERFORM MENU-PRINCIPAL.

            STOP RUN.

        AVALIAR-ACESSO.
            DISPLAY "Insira seu CPF: ".
            ACCEPT WS-CPF.
            DISPLAY "Insira sua senha: ".
            ACCEPT WS-SENHA.    
            MOVE 'N' TO CLIENTE-ENCONTRADO.
            OPEN INPUT CLIENTES.
            PERFORM UNTIL CLIENTE-ENCONTRADO = 'S'
                       OR LEITURA-FINALIZADA = 'S'
                READ CLIENTES
                    AT END
                        MOVE 'S' TO LEITURA-FINALIZADA
                    NOT AT END
                        IF CPF = WS-CPF AND SENHA = WS-SENHA
                            MOVE NOME          TO WS-NOME
                            MOVE SOBRENOME     TO WS-SOBRENOME
                            MOVE SALDO         TO WS-SALDO
                            MOVE EM-EMPRESTIMO TO WS-EM-EMPRESTIMO
                            MOVE LIMITE-EMPRESTIMO
                                               TO WS-LIMITE-EMPRESTIMO
                            MOVE MOVIMENTACOES TO WS-MOVIMENTACOES
                            MOVE 'S'           TO CLIENTE-ENCONTRADO
                        ELSE
                            DISPLAY "Cliente nao encontrado."
                        END-IF
                END-READ
            END-PERFORM.
            CLOSE CLIENTES.
               IF WS-CPF = CPF AND WS-SENHA = SENHA
                  DISPLAY "Acesso concedido. Bem-vindo(a), "
                  DISPLAY WS-NOME
                  DISPLAY " " WS-SOBRENOME
               ELSE
                   DISPLAY "Acesso negado. CPF ou senha incorretos."
                   PERFORM AVALIAR-ACESSO
                END-IF. 


        MENU-PRINCIPAL.
            DISPLAY "Menu Principal:".
            DISPLAY "1. Consultar Saldo".
            DISPLAY "2. Realizar Deposito".
            DISPLAY "3. Realizar Saque".
            DISPLAY "4. Realizar Emprestimo".
            DISPLAY "5. Realizar Transferencia".
            DISPLAY "6. Sair".
            ACCEPT OPCAO.
            PERFORM UNTIL OPCAO = 6
                EVALUATE OPCAO
                    WHEN 1
                        PERFORM CONSULTAR-SALDO
                    WHEN 2
                        PERFORM REALIZAR-DEPOSITO
                    WHEN 3
                        PERFORM REALIZAR-SAQUE
                    WHEN 4
                        PERFORM REALIZAR-EMPRESTIMO
                    WHEN 5
                        PERFORM REALIZAR-TRANSFERENCIA
                    WHEN OTHER
                        DISPLAY "Opcao invalida. Tente novamente."
                 END-EVALUATE
                 DISPLAY "Menu Principal:"
                 DISPLAY "1. Consultar Saldo"
                 DISPLAY "2. Realizar Deposito"
                 DISPLAY "3. Realizar Saque"
                 DISPLAY "4. Realizar Emprestimo"
                 DISPLAY "5. Realizar Transferencia"
                 DISPLAY "6. Sair"
                 ACCEPT OPCAO
            END-PERFORM.    

         CONSULTAR-SALDO.
           CONTINUE.


         REALIZAR-DEPOSITO.
           CONTINUE.


         REALIZAR-SAQUE.
           CONTINUE.

         REALIZAR-EMPRESTIMO.
             CONTINUE.

         REALIZAR-TRANSFERENCIA.
           CONTINUE.
