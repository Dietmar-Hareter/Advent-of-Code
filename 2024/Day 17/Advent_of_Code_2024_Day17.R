setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#data <- readLines("input_day17_Test.txt")
#data <- readLines("input_day17_Test2.txt")
data <- readLines("input_day17.txt")

Reg <- as.numeric(unlist(stringr::str_extract_all(data[1:3], "\\d+")))
Prog <- as.numeric(unlist(stringr::str_extract_all(data[5], "\\d+")))


# Part 1

Combo <- function(Operand, RegA, RegB, RegC) {
  if (Operand >= 0 && Operand <= 3) {
    return(Operand)
  }
  if (Operand == 4) {
    return(RegA)
  }
  if (Operand == 5) {
    return(RegB)
  }
  if (Operand == 6) {
    return(RegC)
  }
}

Run_Program <- function(Reg_in, Prog_in) {
  
  RegA <- Reg_in[1]
  RegB <- Reg_in[2]
  RegC <- Reg_in[3]
  
  Pos <- 1
  Output <- integer(0)

  while (Pos <= length(Prog_in)) {
    Instr <- Prog_in[Pos]
    Operand <- Prog_in[Pos+1]
    
    if (Instr == 0) { # adv
      RegA <- floor(RegA / 2^Combo(Operand, RegA, RegB, RegC))
    } else if (Instr == 1) { # bxl
      RegB <- bitwXor(RegB %% 2^31, Operand %% 2^31)
    } else if (Instr == 2) { # bst
      RegB <- Combo(Operand, RegA, RegB, RegC) %% 8
    } else if (Instr == 3) { # jnz
      if (RegA != 0) {
        Pos <- Operand + 1
        next
      }
    } else if (Instr == 4) { # bxc
      RegB <- bitwXor(RegB %% 2^31, RegC %% 2^31)
    } else if (Instr == 5) { # out
      Output <- c(Output, Combo(Operand, RegA, RegB, RegC) %% 8)
    } else if (Instr == 6) { # bdv
      RegB <- floor(RegA / 2^Combo(Operand, RegA, RegB, RegC))
    } else if (Instr == 7) { # cdv
      RegC <- floor(RegA / 2^Combo(Operand, RegA, RegB, RegC))
    }
    
    Pos <- Pos + 2
  }
  return(Output)
}

Output <- Run_Program(Reg, Prog)
print(paste(Output, collapse = ",")) # Part 1: 4,6,1,4,2,1,3,1,6



# Part 2:

RegA <- 0   # End-Value

for (Pos in 0:15) {
  RegA_Temp <- NULL
  for (Lauf in 1:length(RegA)) {
    RegA_List <- RegA[Lauf] * 8 + 0:7
    for (RegA_Test in RegA_List) {
      Temp <- Run_Program(c(RegA_Test, 0, 0), Prog)
      if (identical(Temp, Prog[(16-Pos):16])) {
        RegA_Temp <- c(RegA_Temp, RegA_Test)
      } 
    }
  }
  RegA <- RegA_Temp
}

print(as.character(min(RegA))) # Part 2: 202366627359274
