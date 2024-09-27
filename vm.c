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

// System call codes
#define EXIT_SC 1            // Exit system call code
#define PRINT_STR_SC 3       // Print string system call code
#define START_TRACING_SC 2046  // Start tracing system call code
#define STOP_TRACING_SC 2047   // Stop tracing system call code

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

    if (print_flag) { // Handle -p flag
        print_program(&header);
        exit(EXIT_SUCCESS);
    }
    else {// Execute the program
        execute_program(&header);
    }
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


    int i = 0, items_in_line = 0;

    for (word_type addr = header->data_start_address; addr <= (header->data_start_address + header->data_length); addr++, i++) {
        if (memory.words[addr] == 0 && memory.words[addr + 1] == 0 && memory.words[addr + 2] == 0) {
            // print ellipses and move to a new line if alrdy has 4 items
            if (items_in_line == 4) {
                printf("%5u: 0\n", addr);  
                printf("     ...\n");      
            } else {
                printf("%5u: 0\t...\n", addr);  
            }

            while (addr <= header->data_start_address + header->data_length && memory.words[addr] == 0) {
                addr++;
            }
            addr--; 
            items_in_line = 0; 
        } else {
            printf("%5u: %d\t", addr, memory.words[addr]);
            items_in_line++;

            if (items_in_line == 5) {
                printf("\n");
                items_in_line = 0;
            }
        }
    }
    printf("\n");
}


// Function to execute the program
void execute_program(const BOFHeader *header) {

    bin_instr_t instr = memory.instrs[PC];
    address_type current_addr = PC;
    print_vm_state(current_addr, instr);
    
    // Ensure the loop runs until the last instruction in the text section
    while (PC < header->text_start_address + header->text_length) {
        bin_instr_t instr = memory.instrs[PC];
        address_type current_addr = PC;

        // If the instruction is EXIT, print and stop the loop
        const char *asm_form = instruction_assembly_form(current_addr, instr);
        if (strcmp(asm_form, "EXIT 0") == 0) {
            // Print the final state before exiting
            print_vm_state(current_addr, instr);
            break;  // Exit loop after processing the EXIT instruction
        }
        

        // Handle tracing if enabled
        if (tracing) {
            print_vm_state(current_addr, instr);
        }

        // Execute the instruction
        execute_instruction(instr);
        

        // Increment PC after processing the instruction
        PC++;
    }
}



// Function to execute a single instruction
void execute_instruction(bin_instr_t instr) {
    instr_type it = instruction_type(instr);

    switch (it) {
        case comp_instr_type:
            switch (instr.comp.func) {
                case ADD_F:
                    registers[instr.comp.rt] = memory.words[registers[instr.comp.rs] + machine_types_sgnExt(instr.immed.offset)] + registers[SP];
                    break;
                case SUB_F:
                    registers[instr.comp.rt] = memory.words[registers[instr.comp.rs] + machine_types_sgnExt(instr.immed.offset)] - registers[SP];
                    break;
                case LWR_F:
                    // Load the word from the calculated memory location into the register
                    registers[instr.comp.rt] = memory.words[registers[instr.comp.rs] + machine_types_sgnExt(instr.immed.offset)];
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
                case SWR_F:
                    // Store the word at the calculated memory location
                    memory.words[registers[instr.comp.rt] + machine_types_sgnExt(instr.immed.offset)] = registers[instr.comp.rs];
                    break;
                default:
                    bail_with_error("Unknown computational function code: %d", instr.comp.func);
                    break;
            }
            break;

        case immed_instr_type:
            switch (instr.immed.op) {
                case ADDI_O:
                    // Calculate the correct offset from $sp
                    word_type offset = machine_types_formOffset(instr.immed.offset);
                    //printf("%d", offset);
                    // Update the memory at GPR[$sp] + offset
                    memory.words[registers[SP] + offset] = instr.immed.immed;
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
                    //printf("Handling custom immediate instruction with opcode %d\n", instr.immed.op);
                    break;
            }
            break;

        case syscall_instr_type:
            handle_system_call(instr);
            break;

        default:
            //bail_with_error("Unknown instruction type: %d", it);
            break;
    }
}

// Function to handle system calls
void handle_system_call(bin_instr_t instr) {
    int code = instruction_syscall_number(instr);  // No enum used here

    switch (code) {
        case EXIT_SC:  // System call to exit the program
            //printf("Exiting program\n");
            exit(0);
        case PRINT_STR_SC:
            //printf("Printing string\n");
            break;
        case START_TRACING_SC:  // Start tracing
            tracing = true;
            //printf("Tracing started\n");
            break;
        case STOP_TRACING_SC:  // Stop tracing
            tracing = false;
            //printf("Tracing stopped\n");
            break;
        default:
            bail_with_error("Unknown system call code: %d", code);
            break;
    }
}

// Function to print the VM's state for tracing
void print_vm_state(address_type addr, bin_instr_t instr) {
    printf("      PC: %u\n", PC);
    for (int i = 0; i < NUM_REGISTERS; i++) { // printing registers
        printf("  GPR[%s]: %d ", regname_get(i), registers[i]);
        if (i%4==0 && i!=0) printf("\n");
    }
    printf("\n");

    for (word_type addr = registers[GP]; addr <= registers[SP]; addr++) {
        if (addr==registers[GP] || addr==registers[SP] || memory.words[addr] != 0) {
            printf("%5u: %d\t", addr, memory.words[addr]);
        }
        else {
            if (addr!=registers[SP]-1 && memory.words[addr+1]==0) {
                addr++;
                while (memory.words[addr]==0 && memory.words[addr+1]==0 && addr!=registers[SP]-1) {
                    addr++;
                }
                printf("...\t");
            }
        }
    }
    printf("\n");

    
    //printf("hello %d", memory.words[4095]);


    const char *asm_form = instruction_assembly_form(addr, instr);
    printf("\n");
    printf("==> %u: %s\n", addr, asm_form);
}


// Function to check VM invariants
bool check_invariants(const BOFHeader *header) {
    if (registers[GP] < 0 || registers[SP] > registers[FP] || PC >= MEMORY_SIZE_IN_WORDS) {
        return false;
    }
    return true;
}