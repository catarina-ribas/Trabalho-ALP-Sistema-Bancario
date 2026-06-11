# Trabalho-ALP-Sistema-Bancario

Uma simples implementação de um sistema bancário na linguagem COBOL para a disciplina de Arquitetura de Linguagens de Programação.

### Como executar o projeto?

**1. Comando para compilar o código COBOL:**
Este comando usa o GnuCOBOL para ler o arquivo de texto (`.cob`) e gerar o programa executável na máquina:

```bash
cobc -x -free -o sistema-bancario sistema-bancario.cob
```

**2.1 Comando para executar o sistema no linux:**
Após a compilação, este comando roda o programa bancário que acabou de ser criado na pasta atual:

```bash
./sistema-bancario
```
**2.2 Comando para executar o sistema no windows(cmd):**

```bash
sistema-bancario
```
**2.3 Comando para executar o sistema no windows(powershell):**

```bash
.\sistema-bancario
```