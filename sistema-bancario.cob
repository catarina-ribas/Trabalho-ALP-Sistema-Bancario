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
           05  SALDO               PIC S9(13)V99 SIGN LEADING SEPARATE.
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
        01  VALOR-TRANSACAO        PIC S9(13)V99 SIGN LEADING SEPARATE.
        01  CPF-DESTINO            PIC X(11).
        01  SALDO-DESTINO          PIC S9(13)V99 SIGN LEADING SEPARATE.
        01  MOVIMENTACOES-DESTINO  PIC 9(04).



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
            MOVE 'N' TO LEITURA-FINALIZADA.
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
                            CONTINUE
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
            DISPLAY "Saldo atual: " WS-SALDO.

         REALIZAR-DEPOSITO.
            OPEN OUTPUT CLIENTES.
            DISPLAY "Digite o valor do deposito: ".
            ACCEPT VALOR-TRANSACAO.
                IF VALOR-TRANSACAO > 0
                    ADD VALOR-TRANSACAO TO WS-SALDO
                    ADD 1 TO WS-MOVIMENTACOES
                    DISPLAY "Deposito realizado. Novo saldo: " WS-SALDO
                ELSE
                    DISPLAY "Valor invalido. Tente novamente."
                END-IF.

         REALIZAR-SAQUE.
            DISPLAY "Digite o valor do saque: ".
            ACCEPT VALOR-TRANSACAO.
                IF VALOR-TRANSACAO > 0 AND VALOR-TRANSACAO <= WS-SALDO
                    SUBTRACT VALOR-TRANSACAO FROM WS-SALDO
                    ADD 1 TO WS-MOVIMENTACOES
                    DISPLAY "Saque realizado. Novo saldo: " WS-SALDO
                ELSE
                    DISPLAY "Valor invalido. Tente novamente."
                END-IF.

         REALIZAR-EMPRESTIMO.
            IF WS-EM-EMPRESTIMO = 'S'
                DISPLAY "Voce ja possui um emprestimo ativo. "
            ELSE
                DISPLAY "Digite o valor do emprestimo: "
                ACCEPT VALOR-TRANSACAO
                    IF VALOR-TRANSACAO > 0 AND 
                    VALOR-TRANSACAO <= WS-LIMITE-EMPRESTIMO
                        ADD VALOR-TRANSACAO TO WS-SALDO
                        MOVE 'S' TO WS-EM-EMPRESTIMO
                        ADD 1 TO WS-MOVIMENTACOES
                        DISPLAY "Concluido. Novo saldo: " WS-SALDO
                    ELSE
                        DISPLAY "Valor invalido ou acima do limite."
                    END-IF
            END-IF.

         REALIZAR-TRANSFERENCIA.
            DISPLAY "Digite o CPF do destinatario: "
            ACCEPT CPF-DESTINO.
            OPEN INPUT CLIENTES.
            MOVE 'N' TO CLIENTE-ENCONTRADO.
            MOVE 'N' TO LEITURA-FINALIZADA.
            PERFORM UNTIL CLIENTE-ENCONTRADO = 'S'
                       OR LEITURA-FINALIZADA = 'S'
                READ CLIENTES
                    AT END
                        MOVE 'S' TO LEITURA-FINALIZADA
                    NOT AT END
                        IF CPF = CPF-DESTINO
                            MOVE SALDO         TO SALDO-DESTINO
                            MOVE MOVIMENTACOES TO MOVIMENTACOES-DESTINO
                            MOVE 'S'           TO CLIENTE-ENCONTRADO
                        ELSE
                            CONTINUE
                        END-IF
                END-READ
                CLOSE CLIENTES
                IF CLIENTE-ENCONTRADO = 'S'
                    DISPLAY "Digite o valor da transferencia: "
                    ACCEPT VALOR-TRANSACAO
                        IF VALOR-TRANSACAO > 0 AND 
                        VALOR-TRANSACAO <= WS-SALDO
                            SUBTRACT VALOR-TRANSACAO FROM WS-SALDO
                            ADD VALOR-TRANSACAO TO SALDO-DESTINO
                            ADD 1 TO WS-MOVIMENTACOES
                            ADD 1 TO MOVIMENTACOES-DESTINO
                            DISPLAY "Concluido. Novo saldo: " WS-SALDO
                        ELSE
                         DISPLAY "Valor invalido ou saldo insuficiente."
                        END-IF
                END-IF
            END-PERFORM.
