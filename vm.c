#include "bof.h"
#include "instruction.h"
#include "machine_types.h"
#include "regname.h"
#include "utilities.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

// Define constants for registers
#define GP 0
#define SP 1
#define FP 2
#define RA 7

// Define the number of general-purpose registers
#define NUM_REGISTERS 8

// Define the size of memory (32K words)
#define MEMORY_SIZE_IN_WORDS 32768

// Define the memory union
union mem_u {
    word_type words[MEMORY_SIZE_IN_WORDS];
    uword_type uwords[MEMORY_SIZE_IN_WORDS];
    bin_instr_t instrs[MEMORY_SIZE_IN_WORDS];
} memory;

// Define VM state
word_type registers[NUM_REGISTERS]; // $gp, $sp, $fp, $r3, $r4, $r5, $r6, $ra
word_type PC;                        // Program Counter
word_type HI, LO;                    // Special registers

// Tracing flag
bool tracing = false;

// Function Prototypes
void load_bof(const char *filename, BOFHeader *header);
void print_program(const BOFHeader *header);
void execute_program(const BOFHeader *header);
void execute_instruction(bin_instr_t instr);
void handle_system_call(bin_instr_t instr);
void print_vm_state(address_type addr, bin_instr_t instr);
bool check_invariants(const BOFHeader *header);

// Main function
int main(int argc, char *argv[]) {
    if (argc < 2) {
        bail_with_error("Usage: %s [-p] <filename.bof>", argv[0]);
    }

    int arg_index = 1;
    bool print_flag = false;

    // Check for -p flag
    if (strcmp(argv[arg_index], "-p") == 0) {
        print_flag = true;
        arg_index++;
        if (argc < 3) {
            bail_with_error("Usage: %s [-p] <filename.bof>", argv[0]);
        }
    }

    const char *filename = argv[arg_index];

    BOFHeader header;

    // Load BOF file
    load_bof(filename, &header);

    // Initialize registers based on header
    registers[GP] = header.data_start_address;
    registers[SP] = header.stack_bottom_addr;
    registers[FP] = header.stack_bottom_addr;
    PC = header.text_start_address;
    HI = 0;
    LO = 0;

    // Check VM invariants
    if (!check_invariants(&header)) {
        bail_with_error("VM invariants violated after initialization.");
    }

    // Handle -p flag
    if (print_flag) {
        print_program(&header);
        exit(EXIT_SUCCESS);
    }

    // Execute the program
    execute_program(&header);

    return 0;
}

// Function to load BOF file into memory
void load_bof(const char *filename, BOFHeader *header) {
    BOFFILE bof = bof_read_open(filename);

    // Read BOF header
    *header = bof_read_header(bof);

    // Load instructions into memory
    for (word_type addr = header->text_start_address; addr < (header->text_start_address + header->text_length); addr++) {
        memory.instrs[addr] = instruction_read(bof);
    }

    // Load data section into memory
    for (word_type addr = header->data_start_address; addr < (header->data_start_address + header->data_length); addr++) {
        memory.words[addr] = bof_read_word(bof);
    }

    bof_close(bof);
}

// Function to print the program in assembly form
void print_program(const BOFHeader *header) {
    printf("Address Instruction\n");
    // Print instructions
    for (word_type addr = header->text_start_address; addr < (header->text_start_address + header->text_length); addr++) {
        bin_instr_t instr = memory.instrs[addr];
        const char *asm_form = instruction_assembly_form(addr, instr);
        printf("%5u: %s\n", addr, asm_form);
    }

    //printf("%u: %d\t", header->data_start_address, memory.words[header->data_start_address]);
    for (word_type addr = header->data_start_address; addr <= (header->data_start_address + header->data_length); addr++) {
        printf("%5u: %d", addr, memory.words[addr]);
        if (addr < header->data_start_address + header->data_length) {
            printf("\t"); // Add tab spacing between data entries
        }

    }
    printf(" ...\n");
}

// Function to execute the program
void execute_program(const BOFHeader *header) {
    while (1) {
        // Ensure PC is within bounds
        if (PC < 0 || PC >= MEMORY_SIZE_IN_WORDS) {
            bail_with_error("PC out of bounds: %d", PC);
        }

        bin_instr_t instr = memory.instrs[PC];
        address_type current_addr = PC;
        PC++; // Increment PC before execution

        // Execute the instruction
        execute_instruction(instr);

        // Handle tracing if enabled
        if (tracing) {
            print_vm_state(current_addr, instr);
        }
    }
}

// Function to execute a single instruction
void execute_instruction(bin_instr_t instr) {
    instr_type it = instruction_type(instr);

    switch (it) {
        case comp_instr_type:
            switch (instr.comp.func) {
                case ADD_F:
                    registers[instr.comp.rt] = registers[instr.comp.rs] + registers[instr.comp.rt];
                    break;
                case SUB_F:
                    registers[instr.comp.rt] = registers[instr.comp.rs] - registers[instr.comp.rt];
                    break;
                case MUL_F:
                    registers[instr.comp.rt] = registers[instr.comp.rs] * registers[instr.comp.rt];
                    break;
                case DIV_F:
                    if (registers[instr.comp.rt] == 0) {
                        bail_with_error("Division by zero.");
                    }
                    registers[instr.comp.rt] = registers[instr.comp.rs] / registers[instr.comp.rt];
                    break;
                default:
                    bail_with_error("Unknown computational function code: %d", instr.comp.func);
                    break;
            }
            break;

        case immed_instr_type:
            switch (instr.immed.op) {
                case ADDI_O:
                    registers[instr.immed.reg] += machine_types_sgnExt(instr.immed.immed);
                    break;
                case ANDI_O:
                    registers[instr.immed.reg] &= machine_types_zeroExt(instr.immed.immed);
                    break;
                case BORI_O:
                    registers[instr.immed.reg] |= machine_types_zeroExt(instr.immed.immed);
                    break;
                case BEQ_O:
                    if (registers[SP] == registers[instr.immed.reg]) {
                        PC += machine_types_formOffset(instr.immed.immed);
                    }
                    break;
                default:
                    printf("Handling custom immediate instruction with opcode %d\n", instr.immed.op);
                    break;
            }
            break;

        case syscall_instr_type:
            handle_system_call(instr);
            break;

        default:
            bail_with_error("Unknown instruction type: %d", it);
            break;
    }
}

// Assign unique system call codes
#define print_str_sc 3  // Assign a unique value, like 3
#define exit_sc 2       // Example for exiting

void handle_system_call(bin_instr_t instr) {
    syscall_type code = instruction_syscall_number(instr);

    // First handle custom system call separately
    if (code == print_str_sc) {
        printf("Printing string\n");
        return;  // Return to avoid executing the rest of the switch statement
    }

    // Use switch statement to handle other cases
    switch (code) {
        case 2:  // System call to exit the program
            printf("Exiting program\n");
            exit(0);
        case 2047:
            printf("Handling custom system call 2047\n");
            break;
        case 2046:
            printf("Handling custom system call 2046\n");
            break;
        default:
            printf("Unknown system call code: %d\n", code);
            break;
    }
}

// Function to print the VM's state for tracing
void print_vm_state(address_type addr, bin_instr_t instr) {
    printf("PC: %u\n", PC);
    printf("Registers:\n");
    for (int i = 0; i < NUM_REGISTERS; i++) {
        printf("  %s: %d ", regname_get(i), registers[i]);
    }
    printf("\n");
    const char *asm_form = instruction_assembly_form(addr, instr);
    printf("==> %u: %s\n", addr, asm_form);
}

// Function to check VM invariants
bool check_invariants(const BOFHeader *header) {
    if (registers[GP] < 0 || registers[SP] > registers[FP] || PC >= MEMORY_SIZE_IN_WORDS) {
        return false;
    }
    return true;
}
