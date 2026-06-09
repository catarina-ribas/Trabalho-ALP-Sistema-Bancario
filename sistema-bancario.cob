>>SOURCE FORMAT FREE
           IDENTIFICATION DIVISION.
           PROGRAM-ID. SISTEMA-BANCARIO.
           
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO 'dados-clientes-cobol.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT OPTIONAL EXTRATO ASSIGN TO 'extrato.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES.
       01  CLIENTE.
           05  ID-CLIENTE           PIC X(05).
           05  CPF                  PIC X(11).
           05  NOME                 PIC X(15).
           05  SOBRENOME            PIC X(20).
           05  SENHA                PIC X(08).
           05  SALDO                PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  EM-EMPRESTIMO        PIC X(01).
           05  LIMITE-EMPRESTIMO    PIC 9(13)V99.
           05  MOVIMENTACOES        PIC 9(04).
       
       FD  EXTRATO.
       01  REG-EXTRATO.
           05  EXT-CPF              PIC X(11).
           05  EXT-OPERACAO         PIC X(15).
           05  EXT-VALOR            PIC S9(13)V99 SIGN LEADING SEPARATE.
       
       WORKING-STORAGE SECTION.
       01  WS-VALOR-TELA            PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-CPF                   PIC X(11).
       01  WS-NOME                  PIC X(15).
       01  WS-SOBRENOME             PIC X(20).
       01  WS-SENHA                 PIC X(08).
       01  WS-SALDO                 PIC S9(13)V99 SIGN LEADING SEPARATE.
       01  WS-EM-EMPRESTIMO         PIC X(01).
       01  WS-LIMITE-EMPRESTIMO     PIC 9(13)V99.
       01  WS-MOVIMENTACOES         PIC 9(04).
       01  LEITURA-FINALIZADA       PIC X(01) VALUE 'N'.
       01  OPCAO                    PIC 9(02).
       01  CLIENTE-ENCONTRADO       PIC X(01) VALUE 'N'.
       01  VALOR-TRANSACAO          PIC S9(13)V99 SIGN LEADING SEPARATE.
       01  CPF-DESTINO              PIC X(11).
       01  SALDO-DESTINO            PIC S9(13)V99 SIGN LEADING SEPARATE.
       01  MOVIMENTACOES-DESTINO    PIC 9(04).
       01  WS-OPCAO-SAIR            PIC X(01) VALUE 'N'.
       
       01  WS-EXT-OPERACAO          PIC X(15).
       01  WS-EXT-VALOR             PIC S9(13)V99 SIGN LEADING SEPARATE.
       
       PROCEDURE DIVISION.
           PERFORM AVALIAR-ACESSO.
           PERFORM MENU-PRINCIPAL.
           STOP RUN.
       
       AVALIAR-ACESSO.
           PERFORM UNTIL CLIENTE-ENCONTRADO = 'S'
               DISPLAY "Insira seu CPF: "
               ACCEPT WS-CPF
               DISPLAY "Insira sua senha: "
               ACCEPT WS-SENHA    
               MOVE 'N' TO CLIENTE-ENCONTRADO
               MOVE 'N' TO LEITURA-FINALIZADA
               OPEN INPUT CLIENTES
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
                                MOVE LIMITE-EMPRESTIMO TO WS-LIMITE-EMPRESTIMO
                                MOVE MOVIMENTACOES TO WS-MOVIMENTACOES
                                MOVE 'S'           TO CLIENTE-ENCONTRADO
                            END-IF
                   END-READ
               END-PERFORM
               CLOSE CLIENTES
               
               IF CLIENTE-ENCONTRADO = 'S'
                  DISPLAY "Acesso concedido. Bem-vindo(a), " WS-NOME " " WS-SOBRENOME
               ELSE
                  DISPLAY "Acesso negado. CPF ou senha incorretos."
                  DISPLAY "Deseja tentar novamente? (S para Sim, N para Nao): "
                  ACCEPT WS-OPCAO-SAIR
                  IF WS-OPCAO-SAIR = 'N' OR WS-OPCAO-SAIR = 'n'
                      STOP RUN
                  END-IF
               END-IF
           END-PERFORM.
       
       MENU-PRINCIPAL.
           PERFORM EXIBIR-OPCOES-MENU.
           ACCEPT OPCAO.
           PERFORM UNTIL OPCAO = 07
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
                   WHEN 6
                       PERFORM CONSULTAR-EXTRATO
                   WHEN OTHER
                       DISPLAY "Opcao invalida. Tente novamente."
                END-EVALUATE
                
                IF OPCAO NOT = 07
                    PERFORM EXIBIR-OPCOES-MENU
                    ACCEPT OPCAO
                END-IF
           END-PERFORM.    
       
       EXIBIR-OPCOES-MENU.
           DISPLAY "---------------------------------------".
           DISPLAY "Menu Principal:".
           DISPLAY "1. Consultar Saldo".
           DISPLAY "2. Realizar Deposito".
           DISPLAY "3. Realizar Saque".
           DISPLAY "4. Realizar Emprestimo".
           DISPLAY "5. Realizar Transferencia".
           DISPLAY "6. Consultar Extrato (Relatorio)".
           DISPLAY "7. Sair".
           DISPLAY "---------------------------------------".
       
       CONSULTAR-SALDO.
    MOVE WS-SALDO TO WS-VALOR-TELA.
    DISPLAY "Saldo atual: R$ " WS-VALOR-TELA.

REALIZAR-DEPOSITO.
    DISPLAY "Digite o valor do deposito: ".
    ACCEPT VALOR-TRANSACAO.
    IF VALOR-TRANSACAO > 0
        ADD VALOR-TRANSACAO TO WS-SALDO
        ADD 1 TO WS-MOVIMENTACOES
        
        
        MOVE WS-SALDO TO WS-VALOR-TELA
        DISPLAY "Deposito realizado. Novo saldo: R$ " WS-VALOR-TELA
        
        MOVE "DEPOSITO" TO WS-EXT-OPERACAO
        MOVE VALOR-TRANSACAO TO WS-EXT-VALOR
        PERFORM REGISTRAR-MOVIMENTACAO
    ELSE
        DISPLAY "Valor invalido. Tente novamente."
    END-IF.

REALIZAR-SAQUE.
    DISPLAY "Digite o valor do saque: ".
    ACCEPT VALOR-TRANSACAO.
    IF VALOR-TRANSACAO > 0 AND VALOR-TRANSACAO <= WS-SALDO
        SUBTRACT VALOR-TRANSACAO FROM WS-SALDO
        ADD 1 TO WS-MOVIMENTACOES
        
        
        MOVE WS-SALDO TO WS-VALOR-TELA
        DISPLAY "Saque realizado. Novo saldo: R$ " WS-VALOR-TELA
        
        MOVE "SAQUE" TO WS-EXT-OPERACAO
        MOVE VALOR-TRANSACAO TO WS-EXT-VALOR
        PERFORM REGISTRAR-MOVIMENTACAO
    ELSE
        DISPLAY "Valor invalido. Tente novamente."
    END-IF.

REALIZAR-EMPRESTIMO.
    IF WS-EM-EMPRESTIMO = 'S'
        DISPLAY "Voce ja possui um emprestimo ativo. "
    ELSE
        DISPLAY "Digite o valor do emprestimo: "
        ACCEPT VALOR-TRANSACAO
        IF VALOR-TRANSACAO > 0 AND VALOR-TRANSACAO <= WS-LIMITE-EMPRESTIMO
            ADD VALOR-TRANSACAO TO WS-SALDO
            MOVE 'S' TO WS-EM-EMPRESTIMO
            ADD 1 TO WS-MOVIMENTACOES
            
            
            MOVE WS-SALDO TO WS-VALOR-TELA
            DISPLAY "Concluido. Novo saldo: R$ " WS-VALOR-TELA
            
            MOVE "EMPRESTIMO" TO WS-EXT-OPERACAO
            MOVE VALOR-TRANSACAO TO WS-EXT-VALOR
            PERFORM REGISTRAR-MOVIMENTACAO
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
                END-IF
        END-READ
    END-PERFORM.
    CLOSE CLIENTES.
    
    IF CLIENTE-ENCONTRADO = 'S'
        DISPLAY "Digite o valor da transferencia: "
        ACCEPT VALOR-TRANSACAO
        IF VALOR-TRANSACAO > 0 AND VALOR-TRANSACAO <= WS-SALDO
            SUBTRACT VALOR-TRANSACAO FROM WS-SALDO
            ADD VALOR-TRANSACAO TO SALDO-DESTINO
            ADD 1 TO WS-MOVIMENTACOES
            ADD 1 TO MOVIMENTACOES-DESTINO
            
            
            MOVE WS-SALDO TO WS-VALOR-TELA
            DISPLAY "Concluido. Novo saldo: R$ " WS-VALOR-TELA
            
            MOVE "TRANSFERENCIA" TO WS-EXT-OPERACAO
            MOVE VALOR-TRANSACAO TO WS-EXT-VALOR
            PERFORM REGISTRAR-MOVIMENTACAO
        ELSE
            DISPLAY "Valor invalido ou saldo insuficiente."
        END-IF
    ELSE
        DISPLAY "Cliente nao encontrado."
    END-IF.

REGISTRAR-MOVIMENTACAO.
    OPEN EXTEND EXTRATO.
    MOVE WS-CPF TO EXT-CPF.
    MOVE WS-EXT-OPERACAO TO EXT-OPERACAO.
    MOVE WS-EXT-VALOR TO EXT-VALOR.
    WRITE REG-EXTRATO.
    CLOSE EXTRATO.

CONSULTAR-EXTRATO.
    DISPLAY " ".
    DISPLAY "=======================================".
    DISPLAY "   RELATORIO DE EXTRATO - EXTRA-TXT    ".
    DISPLAY "=======================================".
    MOVE 0 TO WS-MOVIMENTACOES.
    MOVE 'N' TO LEITURA-FINALIZADA.
    OPEN INPUT EXTRATO.
    PERFORM UNTIL LEITURA-FINALIZADA = 'S'
        READ EXTRATO
            AT END
                MOVE 'S' TO LEITURA-FINALIZADA
            NOT AT END
                IF EXT-CPF = WS-CPF
                    MOVE EXT-VALOR TO WS-VALOR-TELA
                    DISPLAY " OPERACAO: " EXT-OPERACAO " | VALOR: R$ " WS-VALOR-TELA
                    ADD 1 TO WS-MOVIMENTACOES
                END-IF
        END-READ
    END-PERFORM.
    CLOSE EXTRATO.
    DISPLAY "=======================================".
    DISPLAY "Total real de movimentacoes salvas: " WS-MOVIMENTACOES.
    DISPLAY " ".
    