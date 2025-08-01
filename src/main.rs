// ! 18.0505mhz for the average clock speed (about)  or about 55.44ns

/*
TODO's:

    Add the missing functionality for the few specific commands missing it
    Add better debug support (maybe also some debug asserts to ensure safe assembly code)\
    Add support for variable definitions (using @0x01 Testing which would be a label referring to the address; optional constant afterwards)
    Add support for the missing argument types combination
    
    Add a screen; background thread which runs at a specified rate similar to a real vgs monitor
        Use ascii rendering to the terminal to simplify things; zoom out and fullscreen the terminal for complete resolution
    
    Make a connection to the cpu input register that is connected to a keyboard (and possibly mouse) to allow for games and interactions
        Maybe use a specific protocol for requesting and receiving events and specific info like mouse position or something like that


*/

use crossbeam;

mod window;


macro_rules! debug_println {
    ($($arg:tt)*) => (if ::std::cfg!(debug_assertions) { ::std::println!($($arg)*); })
}


pub fn check_tokens (kill: &mut bool, one_liner: &mut bool, char: char, splitters: &Vec <Vec <&str>>) {
    for split in splitters.iter() {
        if char == split[0].chars().nth(0).unwrap() {  *one_liner = true; *kill = true; break;  }
}}
pub fn check_second_token_layer (token: &mut String, kill: &mut bool, line: &String, i: usize, splitters: &Vec <Vec <&str>>) {
    for split in splitters.iter() {
        let mut i2 = 1;
        for s in split {
            if s == token {  *kill = true; break;  }
            i2 += 1;
        } if *kill && i2 < split.len() && i < line.len() - 1 {
            let new = token.clone() + &line[i + 1..=i+1];
            if split.contains(&new.as_str()) {  *kill = false;  }
}}}
pub fn handle_inner (tokens: &mut Vec <Vec <String>>, line: String, splitters: &Vec <Vec <&str>>) {
    let mut token = String::new();
    let mut layer = vec![];
    for (i, char) in line.clone().chars().enumerate() {
        let mut kill = false;
        let mut one_liner = false;
        check_tokens(&mut kill, &mut one_liner, char, splitters);
        check_second_token_layer(&mut token, &mut kill, &line, i, splitters);
        if one_liner && !token.is_empty() {  layer.push(token.to_string()); token.clear();  }
        token.push(char);
        if kill {  layer.push(token.to_string()); token.clear();  }
    } layer.push(token.to_string()); tokens.push(layer);
}
pub fn remove_comments (tokens: &mut Vec <Vec <String>>) {
    for i in 0..tokens.len() {
        let mut in_comment = false;
        let mut non_comments = vec![];
        let token_line = &tokens[i];
        for token in token_line {
            if token == "//" || token == ";" {  in_comment = true;  }
            if !in_comment && !token.is_empty() {  non_comments.push(token.clone());  }
        } tokens[i] = non_comments;
    } let mut new_tokens = vec![];
    for line in &mut *tokens {
        if !line.is_empty() {  new_tokens.push(line.clone());  }
    } *tokens = new_tokens;
}
pub fn tokenize (text: Vec <String>, splitters: &Vec <Vec <&str>>) -> Vec <Vec <String>> {
    let mut tokens = vec![];
    for line in text {  handle_inner(&mut tokens, line, splitters);  }
    remove_comments(&mut tokens);
    remove_extras(&mut tokens); tokens
}
// removing the final extra tokens to clean up the result
pub fn remove_extras (tokens: &mut Vec <Vec <String>>) {
    for layer in tokens.iter_mut() {
        let mut valid_tokens = vec![];
        for token in layer.iter() {
            if !(token.is_empty() || matches!(token.as_str(), " " | "," | "[" | "]" | "%" | "#" | "$")) {
                valid_tokens.push(token.clone());
            }
        } *layer = valid_tokens;
    }
    
    // removing blank lines
    let mut index = 0;
    while index < tokens.len() {
        if tokens[index].is_empty() {
            tokens.remove(index);
            continue;
        }
        index += 1;
    }
}
#[repr(u8)]
#[derive(Debug, Clone)]
pub enum ArgType {
    Null     = 0,
    Register = 1,
    Address  = 2,
    Number   = 3,
    Line     = 4
}
// all the instructions and their byte representation and info       no instructions use more than 2 args (at least so far; um...... sure)
static INSTRUCTION_REFERENCE: [(u8, &str, ArgType, ArgType, ArgType); 79] = [
    (0x00u8, "nop"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x11u8, "add"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x12u8, "addC"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x13u8, "sub"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x14u8, "subC"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x15u8, "or"    , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x16u8, "and"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x17u8, "xor"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x18u8, "cmpg"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x19u8, "cmpl"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x1Au8, "inc"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x1Bu8, "dec"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x1Cu8, "left"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x1Du8, "right" , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x1Eu8, "rotl"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x1Fu8, "rotr"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x21u8, "mov"   , ArgType::Register, ArgType::Register, ArgType::Null    ),
    (0x22u8, "str"   , ArgType::Register, ArgType::Address , ArgType::Null    ),
    (0x23u8, "grab"  , ArgType::Address , ArgType::Register, ArgType::Null    ),
    (0x24u8, "ldrar" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x25u8, "ldral" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x26u8, "movreg", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x27u8, "movram", ArgType::Address , ArgType::Null    , ArgType::Null    ),
    (0x28u8, "ldrp"  , ArgType::Register, ArgType::Register, ArgType::Null    ),
    (0x29u8, "strp"  , ArgType::Register, ArgType::Register, ArgType::Null    ),
    (0x2Au8, "ldi"   , ArgType::Register, ArgType::Number  , ArgType::Null    ),
    (0x2Bu8, "ldiar" , ArgType::Number  , ArgType::Null    , ArgType::Null    ),
    (0x2Cu8, "ldial" , ArgType::Number  , ArgType::Null    , ArgType::Null    ),
    (0x31u8, "out"   , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x32u8, "inp"   , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x33u8, "retio" , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x34u8, "cmpio" , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x35u8, "setout", ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x41u8, "psh"   , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x42u8, "pop"   , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x43u8, "popptr", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x44u8, "pshptr", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x45u8, "pshrm" , ArgType::Address , ArgType::Null    , ArgType::Null    ),
    (0x46u8, "poprm" , ArgType::Address , ArgType::Null    , ArgType::Null    ),
    (0x47u8, "strst" , ArgType::Register, ArgType::Address , ArgType::Null    ),
    (0x48u8, "ldrst" , ArgType::Address , ArgType::Register, ArgType::Null    ),
    (0x49u8, "cpysp" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x4Au8, "setsp" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x4Bu8, "pshcon", ArgType::Number  , ArgType::Null    , ArgType::Null    ),
    (0x4Cu8, "clrstk", ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x51u8, "jmp"   , ArgType::Line    , ArgType::Null    , ArgType::Null    ),
    (0x52u8, "jnz"   , ArgType::Line    , ArgType::Null    , ArgType::Null    ),
    (0x53u8, "retz"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x54u8, "jiz"   , ArgType::Line    , ArgType::Null    , ArgType::Null    ),
    (0x55u8, "jmpr"  , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x56u8, "jnc"   , ArgType::Line    , ArgType::Null    , ArgType::Null    ),
    (0x57u8, "jic"   , ArgType::Line    , ArgType::Null    , ArgType::Null    ),
    (0x58u8, "ret"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x59u8, "pshpc" , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x5Au8, "cpypc" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x5Bu8, "jmpptr", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x5Cu8, "jmpad" , ArgType::Address , ArgType::Null    , ArgType::Null    ),
    (0x61u8, "halt"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x62u8, "setbuf", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x63u8, "blkcpy", ArgType::Register, ArgType::Register, ArgType::Number  ),
    (0x64u8, "sptcpy", ArgType::Register, ArgType::Register, ArgType::Number  ),
    (0x65u8, "cycles", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x66u8, "sync"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x71u8, "flags" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x72u8, "clr"   , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x73u8, "clrmem", ArgType::Address , ArgType::Null    , ArgType::Null    ),
    (0x74u8, "cmpeq" , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x75u8, "cmp"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x76u8, "flgzro", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x77u8, "flgcry", ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x78u8, "memcmp", ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x79u8, "movz"  , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x7Au8, "movnz" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x7Bu8, "movc"  , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x7Cu8, "movnc" , ArgType::Register, ArgType::Null    , ArgType::Null    ),
    (0x7Du8, "memcpy", ArgType::Register, ArgType::Register, ArgType::Number  ),
    (0x7Eu8, "ramswp", ArgType::Register, ArgType::Register, ArgType::Null    ),
    (0x81u8, "mult"  , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
    (0x82u8, "div"   , ArgType::Null    , ArgType::Null    , ArgType::Null    ),
];
// the final assembly step; all macros are inlined and variables are written in as addresses
// all '$', '#', ',', and ' ' tokens should be pruned to keep the argument format pure allowing easy abstractions and thus easy assembling
pub fn get_instruction (token_line: &Vec <String>, instruction_size_table: &[u8]) -> (u32, u8) {
    if token_line.is_empty() { return (0, 0); }  // nop instruction for null lines (shouldn't happen)
    let op_code = token_line[0].as_str();
    let mut op_code_byte = None;
    let mut index = 0;  // index 0 maps to nop so if none are found it should default back just fine
    for (i, (byte_code, instruction, _arg_1, _arg_2, _arg_3)) in INSTRUCTION_REFERENCE.iter().enumerate() {
        if *instruction == op_code {
            op_code_byte = Some(*byte_code);
            index = i;
            break;
        }
    }
    let op_code_byte = op_code_byte.expect(&format!("Invalid instruction; unable to reference the specified instruction from the lookup tabel. {:?}", token_line));
    let size = instruction_size_table[op_code_byte as usize] - 1;
    if size == 0 {
        //println!("Size zero; {}; {:?}", op_code_byte, token_line);
        
        return ((op_code_byte as u32) << 24, size);  }  // 1 byte instruction, no operands
    // going through the operands
    let shifted_byte_code = (op_code_byte as u32) << 24;
    let (_, _, arg_1, arg_2, arg_3) = INSTRUCTION_REFERENCE[index].clone();
    let byte_code_form = match (arg_1, arg_2, arg_3) {
        (ArgType::Register, ArgType::Register                             , ArgType::Null) => {  // + 1 byte     <inst> <%><reg><,> <%><reg>
            shifted_byte_code | register_as_byte(&token_line[1]) << 20 | register_as_byte(&token_line[2]) << 16
        },
        (ArgType::Register, ArgType::Null                                 , ArgType::Null) => {  // + 1 byte     <inst> <%><reg>
            shifted_byte_code | register_as_byte(&token_line[1]) << 20
        },
        (ArgType::Register, ArgType::Number|ArgType::Address|ArgType::Line, ArgType::Null) => {  // + 3 bytes    <inst> <%><reg><,> <$|#><value>
            shifted_byte_code | register_as_byte(&token_line[1]) << 20 | token_line[2].parse::<u32>().unwrap()
        },
        (ArgType::Number|ArgType::Address|ArgType::Line, ArgType::Register, ArgType::Null) => {  // + 3 bytes    <inst> <$|#><value><,> <%><reg>
            shifted_byte_code | token_line[1].parse::<u32>().unwrap() << 8 | register_as_byte(&token_line[2]) << 4
        },
        (ArgType::Number|ArgType::Address|ArgType::Line, ArgType::Null    , ArgType::Null) => {  // + 2 bytes    <inst> <$|#><value>
            shifted_byte_code | token_line[1].parse::<u32>().unwrap() << 8
        },  // todo!!!! add the others    // reg, reg, num combo is missing
        _ => 0  // nop
    };
    //println!("Info:: {:x}/{}  / {}     code: {:x}({})/{:x} size: {}", byte_code_form, size, index, op_code_byte, op_code, shifted_byte_code, instruction_size_table[op_code_byte as usize]);
    (byte_code_form, size)  // halt takes no arguments so it shouldn't get to here
    // number, reg|null
    // reg, num|null|reg
}
static REGISTER_ENCODINGS: [(&str, u32); 15] = [
    ("rda", 0x1),
    ("rdb", 0x2),
    ("rdc", 0x3),
    ("rdd", 0x4),
    ("rde", 0x5),
    ("rdf", 0x6),
    ("rdg", 0x7),
    ("rdh", 0x8),
    ("rdi", 0x9),
    ("rdj", 0xA),
    ("rdk", 0xB),
    ("rdl", 0xC),
    ("stp", 0xD),
    ("pgc", 0xE),
    ("acc", 0xF),
];
pub fn register_as_byte (register: &str) -> u32 {
    for reg in REGISTER_ENCODINGS.iter() {
        if reg.0 == register {  return reg.1;  }
    } panic!("Invalid register name; {}", register)  // this shouldn't happen (unless you used nop for some unknown reason as a register???)
}
#[derive(Debug)]
pub struct Macro {
    function_name: String,
    argument_names: Vec<String>,
    expansion: Vec<Vec<String>>,
} impl Macro {
    pub fn expand (&self, input_args: Vec<String>, starting_byte: usize) -> Vec<Vec<String>> {
        //println!("({}) Expansion planned: <{:?}>", self.function_name, self.expansion);
        
        let mut expansion = vec![];
        for line in &self.expansion {
            let mut line_tokens = vec![];
            for token in line {
                if let Some(index) = self.argument_names.iter().position(|x| x == token) {
                    line_tokens.push(input_args[index].clone());  // getting the argument
                    continue;
                } else if let Some(extension) = token.get(..6) {
                    //println!("extension: {}", extension);
                    if extension == "_data_" {
                        let offset = token.get(7..token.len()).unwrap_or("0");
                        line_tokens.push(format!("{}", starting_byte + offset.parse::<usize>().unwrap()));
                        continue;
                    }
                } line_tokens.push(token.clone());
            } expansion.push(line_tokens);
        } expansion
    }
}
pub fn get_macro_lines (tokens: &mut Vec<Vec<String>>) -> Vec<(usize, String, Vec<String>)> {
    let mut macro_lines = vec![];
    for (index, line) in tokens.iter().enumerate() {
        if line[0] != *"macro!" {  continue;  }
        let mut args = vec![];
        let mut in_parameter = false;
        for token in line {
            if *token == *"(" {  in_parameter = true;  continue;  }
            if *token == *")" {  break;  }
            if in_parameter {  // commas have already been pruned
                args.push(token.to_owned());
            }
        }
        macro_lines.push((index, line[1].to_owned(), args));
    } println!("Macros: {:?}", macro_lines);
    macro_lines
}
pub fn get_macros (tokens: &mut Vec<Vec<String>>, instruction_size_table: &[u8], mut macros: Vec<Macro>) {
    // getting the macro lines
    let macro_lines = get_macro_lines(tokens);
    println!("Macro lines: {:?}", macro_lines);
    
    // getting all info on the macros (aka the expansion; followed by removing the definition)
    let mut cut = 0;
    for macro_line in macro_lines {
        // searching for the end
        let mut end_line = None;
        let mut new_cut = 0;
        for line_index in macro_line.0 - cut..tokens.len() {
            new_cut += 1;
            if !tokens[line_index].contains(&String::from("}")) {  continue;  }
            end_line = Some(line_index);
            break;
        } let end_line = end_line.unwrap();  // an error would only be thrown if the programmer failed to close the brackets
        let output: Vec<Vec<String>> = tokens.drain(macro_line.0 - cut..=end_line).collect();
        let output = output[1..output.len() - 1].to_vec();
        let mac = Macro {
            function_name: macro_line.1,
            argument_names: macro_line.2,
            expansion: output,
        }; macros.push(mac);
        cut += new_cut;
    } println!("Macs!!!! {:?}", macros);
    
    //println!("Macro info: <{:?}>", macros);
    
    // expanding all existing instances of the macros
    let mut macro_lines = vec![];
    for (index, line) in tokens.iter_mut().enumerate() {
        let mut contained = (false, 0);
        for (macro_index, mac) in macros.iter().enumerate() {
            if line.contains(&mac.function_name) {
                contained = (true, macro_index);
                break;
            }
        } if !contained.0 {  continue;  }
        macro_lines.push(index);
        
        //println!("usage {:?}? line: {}  code: {:?}", contained, index, line);
    }
    
    // returns a vector with each line containing (optionally) the name the header (defined or jumping to) and the type of jump
    let _jumps = clear_extra_info (tokens, macro_lines, macros, instruction_size_table);
    // using the jump information finish filling those sections out in the byte creation part to ensure proper byte alignment (the hard part)
    // the jump filling is being done in clear_extra_info
}

#[derive(Debug)]
pub enum LineJumpType {
    Header,
    Jump,
    Null
}

pub fn handle_macros (index: &mut usize,
                      tokens: &mut Vec<Vec<String>>,
                      macros: &Vec<Macro>,
                      _macro_lines: &Vec<usize>,
                      byte_index: &mut usize,
                      replace: &mut bool,
                      byte_size: usize,
) -> bool {
    //if *replace {  return false;  }
    let macro_index = macros
                                .iter()
                                .position(|mac| mac.function_name == tokens[*index][0]);
    if macro_index.is_none() {  return false;  }
    //println!("Old tokens: <{:?}>", tokens);
    
    let macro_index = macro_index.unwrap();
    let mut expanded = macros[macro_index].expand(tokens[*index][1..].to_vec(), *byte_index);
    //println!("Expansion: <{:?}>", expanded);
    tokens.remove(*index);
    for _ in 0..expanded.len() {
        tokens.insert(*index, expanded.pop().unwrap());
    }
    //*index -= 1;  // decrementing the index so that the byte count will actually include the first expanded value
    //*replace = true;  // making sure there isn't an extra jump value inserted (whatever previous value should still be fine)
    //println!("New tokens: <{:?}>", tokens);
    
    *byte_index -= byte_size;  // making sure the bytes don't get misaligned
    
    true
}

pub fn clear_extra_info (
    tokens: &mut Vec<Vec<String>>,
    macro_lines: Vec<usize>,
    macros: Vec<Macro>,
    instruction_size_table: &[u8],
) -> Vec<Option<(String, LineJumpType, usize)>> {
    // the macros have to be expanded first before headers can be computed
    // macros and headers can be computed at the same time
    
    let mut byte_index = 0;
    let mut jumps = vec![];  // technically just headers....
    let mut jump_names = vec![];  // technically header names.....
    let mut index = 0; let mut replace = false;
    while index < tokens.len() {
        let line = &tokens[index];
        //println!("Current line {}: {:?} / {:?}", index, line, tokens);
        
        if line.get(0).unwrap_or(&String::new()) == "." && line.get(2).unwrap_or(&String::new()) == ":" {
            let name = line[1].to_owned();
            jump_names.push(name.to_owned());
            replace = true;
            jumps.push(Some((name, LineJumpType::Header, byte_index)));
            tokens.remove(index);  // index isn't incremented afterward so this will be kept in sync
            continue;
        }
        
        // incrementing the byte index
        let instruction = INSTRUCTION_REFERENCE.iter().position(|(_byte, code, _, _, _)| {
            *code == tokens[index][0]
        });
        if instruction.is_none() {  println!("Failed... {:?}", tokens[index])  }
        // technically the unwrap_or would lead to add being defaulted too, although add and nop have the same size so it doesn't matter
        let byte_count = instruction_size_table[INSTRUCTION_REFERENCE[instruction.unwrap_or(0)].0 as usize] as usize;
        let was_expanded = handle_macros(&mut index, tokens, &macros, &macro_lines, &mut byte_index, &mut replace, byte_count);
        byte_index += byte_count;
        //println!("Current Byte Count: {}    instruction: {:?}", byte_index, tokens[index]);
        if was_expanded && !replace {  continue;  }
        
        index += 1;
        if !replace {
            jumps.push(None);
            continue;
        }
        replace = false;
    }
    
    //println!("Jumps: {:?}", jumps);
    
    // replacing headers with their byte indexes (within jump statements or otherwise)
    for slice in tokens.iter_mut() {
        while let Some(position) = slice.iter().position(|value| jump_names.contains(value)) {
            let name = slice[position].to_owned();
            //println!("name: {}, names: {:?}", name, jump_names);
            let byte_index = jumps[*jumps
                                       .iter()
                                       .position(|value|
                                           value.is_some() && value.as_ref().unwrap().0 == name
                                       ).as_ref()
                                       .unwrap()
            ].as_ref().unwrap().2;
            slice[position] = byte_index.to_string();
        }
    }
    
    jumps
}

pub fn replace_jump_byte (tokens: &mut Vec<Vec<String>>, jumps: Vec<Option<(String, LineJumpType)>>, byte_count: usize) {
    for (index, line) in tokens.iter_mut().enumerate() {
        for (token_index, token) in line.iter().enumerate() {
            if jumps.iter()
                    .position(|x| x.as_ref().unwrap_or(&(String::new(), LineJumpType::Null)).0 == *token)
                    .is_some()
            {
                let position = byte_count;
                line[token_index] = position.to_string();
                break;
            }
        }
    }
}

pub fn generate_instructions (
    instructions: &mut [u8],
    instruction_size_table: &[u8],
    tokens: &mut Vec<Vec<String>>,
    token_ref: &mut [usize]
) {
    let macros: Vec<Macro> = vec![
        // the base assembler level macros (the programmer can define their own in the program file)
        Macro {  // call label
            function_name: String::from("call"),
            argument_names: vec![String::from("label_name")],
            expansion: vec![
                vec![String::from("pshcon"), String::from("_data_+6")],  // _data_ is a the keyword for the byte number at assemble time; +6 is the optional argument for offsetting it
                vec![String::from("jmp"), String::from("label_name")]
            ],
        },
        Macro {  // not register
            function_name: String::from("not"),
            argument_names: vec![String::from("register")],
            expansion: vec![
                vec![String::from("ldral"), String::from("register")],
                vec![String::from("ldiar"), String::from("0xFFFF")],
                vec![String::from("xor")]
            ],
        },
        Macro {  // offset reg, offset, pointer_reg
            function_name: String::from("offset"),
            argument_names: vec![String::from("base_register"), String::from("offset_amount"), String::from("pointer_output_register")],
            expansion: vec![
                vec![String::from("ldiar"), String::from("offset_amount")],
                vec![String::from("ldral"), String::from("base_register")],
                vec![String::from("add")],
                vec![String::from("movreg"), String::from("pointer_output_register")],
            ]
        }
    ];
    
    get_macros(tokens, instruction_size_table, macros);
    println!("After macro expansion: {:?}", tokens);
    
    // iterating and generating the instructions
    let mut line_count = 0;
    let mut instruction_byte = 0;
    let mut haulted = false;
    for token_line in tokens {
        let (instruction, instruction_size) = get_instruction(token_line, instruction_size_table);
        if token_line.contains(&String::from("halt")) {  haulted = true;  }
        //println!("{:x}; {:x}", instruction, instruction_size);
        
        // size = 0 is 1 byte, size = 3 is 4 bytes (programmer indexes.........)
        //println!("Line count: {} Byte count: {} Instruction: hex{:x}({}) {:?}", line_count, instruction_byte, instruction, instruction_size, token_line);
        for i in 0..=instruction_size {
            let byte = ((instruction & (0xFF000000 >> (i*8))) >> (24 - i*8)) as u8;
            print!("{:02x} ", byte);
            instructions[instruction_byte] = byte;  // only loading the one byte meaning all data after the program should be nulled out
            token_ref[instruction_byte] = line_count;
            instruction_byte += 1;
        }
        line_count += 1;
        println!();
    } assert!(haulted, "The program requires a hault instruction to ensure it won't overflow the ROM.");
}
fn main() {
    //println!("Starting high-priority emulator");
    
    emulation_loop();
}

pub fn emulation_loop() {
    // Warmup loop before the timer
    for _ in 0..1000 {
        std::hint::black_box(0);
    }
    
    let start = now_nanoseconds();
    while now_nanoseconds() - start < 100000 {}
    let end = now_nanoseconds();
    println!("{} / {} == {}", start, end, end - start);
    
    let start = std::time::Instant::now();
    let end = start.elapsed().as_nanos();
    println!("{}", end);
    
    let splitters = vec![
        vec!["."], vec!["@"], vec!["#"], vec!["$"], vec!["&"], vec!["["], vec!["]"], vec![" "],
        vec![":"], vec![","], vec![";"], vec!["{"], vec!["}"], vec!["%"], vec!["("],
        vec![")"]
    ];
    
    let code = vec![
        String::from("ldi    %rda, $5 ;"),
        String::from("ldiar  $5       ;"),
        String::from("ldral  %rda     ;"),
        String::from("add             ;"),
        String::from("movreg %rdb     ;"),
        String::from("jnz    #20      ;"),
        String::from("nop             ;"),
        String::from("macro! increment (%reg) {             ;"),
        String::from("    ldral %reg             ;"),
        String::from("    inc             ;"),
        String::from("    movreg %reg             ;"),
        String::from("}             ;"),
        String::from("nop             ;"),
        String::from("nop             ;"),
        String::from("macro! double (%reg) {"),
        String::from("    ldral  %reg     ;"),
        String::from("    ldrar  %reg     ;"),
        String::from("    add             ;"),
        String::from("    movreg %reg     ;"),
        String::from("    inc %reg     ;"),
        String::from("    nop             ;"),
        String::from("}               ;"),
        String::from("nop             ;"),
        String::from("nop             ;"),
        String::from("nop             ;"),
        String::from("double %rda     ;"),
        String::from("nop             ;"),
        String::from("ldral %rdb      ;"),
        String::from("ldrar %rdb      ;"),
        String::from("add             ;"),
        String::from("movram #10      ;"),
        String::from("nop             ;"),
        String::from("cycles %rdd     ;"),
        String::from("                ;"),
        String::from("                ;"),
        String::from("ldi %rda, $5    ;"),
        String::from(".label:         ;"),
        String::from("    ldral %rda  ;"),
        String::from("    ldiar $1    ;"),
        String::from("    sub         ;"),
        String::from("    movreg %rda ;"),
        String::from("    jnz label   ;"),
        String::from("                ;"),
        String::from("nop             ;"),
        String::from("ldi %rdd $15    ;"),
        String::from("double %rdd     ;"),
        String::from("jmp next        ;"),
        String::from("nop             ;"),
        String::from("nop             ;"),
        String::from("ldiar $100      ;"),
        String::from(".function_label:;"),
        String::from("    ldi %rdf $10    ;"),
        String::from("    nop             ;"),
        String::from("    ret             ;"),
        String::from("nop             ;"),
        String::from("nop             ;"),
        String::from("ldiar $100      ;"),
        String::from(".next:          ;"),
        String::from("call function_label;"),
        String::from("nop             ;"),
        String::from("halt            ;"),
    ];

    let code = std::fs::read_to_string("/Users/Andrew/Desktop/Programing/Rust/AssemblerV2/scripts/pong.asm2")
        .unwrap().lines().map(|s| s.to_string()).collect();
    println!("{:?}", code);
    
    let mut tokens = tokenize(code, &splitters);
    println!("Text: {:?}", tokens);
    
    // the table of byte aligned instructions
    let mut instructions = [0u8; u16::MAX as usize+1];  // plus one because the size is one larger than the index
    
    // assembling the instructions
    
    // lookup based on the value of the first byte (first 8 - bits)
    // that will determine the instructions size in bytes (to allow correct movement to the following instructions)
    // (default is 1 so that NOP still increment the counter and not stall)
    let mut instruction_size_table: [u8; u8::MAX as usize + 1] = [1u8; u8::MAX as usize+1];  // plus one because the size is one larger than the index
    
    // loading the instruction size table (indexed by the byte code/first byte of the instruction)... (in brackets to allow collapsing)
    {
        instruction_size_table[0x11] = 1;
        instruction_size_table[0x12] = 1;
        instruction_size_table[0x13] = 1;
        instruction_size_table[0x14] = 1;
        instruction_size_table[0x15] = 1;
        instruction_size_table[0x16] = 1;
        instruction_size_table[0x17] = 1;
        instruction_size_table[0x18] = 1;
        instruction_size_table[0x19] = 1;
        instruction_size_table[0x1A] = 1;
        instruction_size_table[0x1B] = 1;
        instruction_size_table[0x1C] = 1;
        instruction_size_table[0x1D] = 1;
        instruction_size_table[0x1E] = 1;
        instruction_size_table[0x1F] = 1;
        instruction_size_table[0x21] = 2;
        instruction_size_table[0x22] = 4;
        instruction_size_table[0x23] = 4;
        instruction_size_table[0x24] = 2;
        instruction_size_table[0x25] = 2;
        instruction_size_table[0x26] = 2;
        instruction_size_table[0x27] = 3;
        instruction_size_table[0x28] = 2;
        instruction_size_table[0x29] = 2;
        instruction_size_table[0x2A] = 4;
        instruction_size_table[0x2B] = 3;
        instruction_size_table[0x2C] = 3;
        instruction_size_table[0x31] = 2;
        instruction_size_table[0x32] = 2;
        instruction_size_table[0x33] = 1;
        instruction_size_table[0x34] = 1;
        instruction_size_table[0x35] = 1;
        instruction_size_table[0x41] = 2;
        instruction_size_table[0x42] = 2;
        instruction_size_table[0x43] = 2;
        instruction_size_table[0x44] = 2;
        instruction_size_table[0x45] = 3;
        instruction_size_table[0x46] = 3;
        instruction_size_table[0x47] = 4;
        instruction_size_table[0x48] = 4;
        instruction_size_table[0x49] = 2;
        instruction_size_table[0x4A] = 2;
        instruction_size_table[0x4B] = 3;
        instruction_size_table[0x4C] = 1;
        instruction_size_table[0x51] = 3;
        instruction_size_table[0x52] = 3;
        instruction_size_table[0x53] = 1;
        instruction_size_table[0x54] = 3;
        instruction_size_table[0x55] = 2;
        instruction_size_table[0x56] = 3;
        instruction_size_table[0x57] = 3;
        instruction_size_table[0x58] = 1;
        instruction_size_table[0x59] = 1;
        instruction_size_table[0x5A] = 2;
        instruction_size_table[0x5B] = 2;
        instruction_size_table[0x5C] = 3;
        instruction_size_table[0x61] = 1;
        instruction_size_table[0x62] = 3;
        instruction_size_table[0x63] = 4;
        instruction_size_table[0x64] = 4;
        instruction_size_table[0x65] = 2;
        instruction_size_table[0x71] = 2;
        instruction_size_table[0x72] = 2;
        instruction_size_table[0x73] = 3;
        instruction_size_table[0x74] = 1;
        instruction_size_table[0x75] = 1;
        instruction_size_table[0x76] = 2;
        instruction_size_table[0x77] = 2;
        instruction_size_table[0x78] = 1;
        instruction_size_table[0x79] = 2;
        instruction_size_table[0x7A] = 2;
        instruction_size_table[0x7B] = 2;
        instruction_size_table[0x7C] = 2;
        instruction_size_table[0x7D] = 4;
        instruction_size_table[0x7E] = 2;
        // mult and div r just 1; too lazy to add
    }
    
    let mut instruction_refs = [0usize; u16::MAX as usize+1];
    generate_instructions(&mut instructions, &instruction_size_table, &mut tokens, &mut instruction_refs);
    
    // all the registers, ram, and the stack
    static REGISTER_RDA: u16 = 0;
    static REGISTER_RDB: u16 = 0;
    static REGISTER_RDC: u16 = 0;
    static REGISTER_RDD: u16 = 0;
    static REGISTER_RDE: u16 = 0;
    static REGISTER_RDF: u16 = 0;
    static REGISTER_RDG: u16 = 0;
    static REGISTER_RDH: u16 = 0;
    static REGISTER_RDI: u16 = 0;
    static REGISTER_RDJ: u16 = 0;
    static REGISTER_RDK: u16 = 0;
    static REGISTER_RDL: u16 = 0;
    static REGISTER_STP: u16 = 0;
    static REGISTER_PGC: u16 = 0;
    static REGISTER_ACC: u16 = 0;
    
    static REGISTER_ALU_LEFT: u16 = 0;
    static REGISTER_ALU_RIGHT: u16 = 0;
    static REGISTER_ALU_OUT: u16 = 0;
    
    static REGISTER_IO_INPUT: u16 = 0;
    static REGISTER_IO_OUTPUT: u16 = 0;
    static REGISTER_IO_INPUT_FLAG: u16 = 0;
    static REGISTER_IO_OUTPUT_FLAG: u16 = 0;
    
    let mut ram = [0u16; u16::MAX as usize+1];
    let mut stack = [0u16; u16::MAX as usize+1];  // very large stack...... what ever though
    
    // flags
    let mut carry = false;
    let mut zero = false;
    
    let mut registers: [u16; 23] = [0,  // the first element is null and shouldn't end up being used
        REGISTER_RDA, REGISTER_RDB, REGISTER_RDC, REGISTER_RDD, REGISTER_RDE,
        REGISTER_RDF, REGISTER_RDG, REGISTER_RDH, REGISTER_RDI, REGISTER_RDJ,
        REGISTER_RDK, REGISTER_RDL, REGISTER_STP, REGISTER_PGC, REGISTER_ACC,
        REGISTER_ALU_LEFT, REGISTER_ALU_RIGHT, REGISTER_ALU_OUT,
        REGISTER_IO_INPUT, REGISTER_IO_OUTPUT, REGISTER_IO_INPUT_FLAG, REGISTER_IO_OUTPUT_FLAG
    ];
    // making sure the cpu has hopefully cached the registers and other values for improved performance
    instructions[0] = instructions[0];
    registers[0] = registers[0]; registers[10] = registers[10];
    
    // the screen is 128x80
    // the current value here is just the default, an instruction can change it to allow different locations and dooble buffering
    let mut screen_buffer_pointer = 55295;  // max ram address - screen_area
    
    ram[screen_buffer_pointer] = 0b10000_000000_00000; ram[screen_buffer_pointer + 2] = 0b00000_100000_00000;
    ram[screen_buffer_pointer + 4] = 0b00000_000000_10000; ram[screen_buffer_pointer + 6] = 0b11111_000000_00000;
    
    // this is set by the screen on the start of a frame
    // this is only reset upon an instruction reading its value
    let mut frame_signal = false;  // add an instruction for this so vsync style things can be done to prevent screen tearing to an extent
    
    
    
    // getting raw pointers to the screen_buffer_pointer and ram
    let buffer_pointer = window::RawPtr::new(&mut screen_buffer_pointer as *mut usize);
    let ram_pointer = window::RawPtr::new(ram.as_mut_ptr());
    let frame_start_signal_pointer = window::RawPtr::new(&mut frame_signal as *mut bool);

    // creating channels to send/receive communication messages for shutting down correctly
    let (sender, receiver) = crossbeam::channel::bounded::<bool>(1);
    for _ in 0..120 {  println!("");  }
    let thread = std::thread::spawn(move || {
        screen_thread(ram_pointer, buffer_pointer, frame_start_signal_pointer, receiver);
        // once the thread dies, all the refs to the pointers are dropped
        // as long as the thread is joined before the completion of the function, memory safety is guaranteed
        // race conditions or overwrites, however, are not safe unless double buffering is correctly done (even then
        // there's a small chance of overlap, although the artifacts are an output and never reused so they won't propigate or cause issues).
        // ...
    });
    //std::thread::sleep(std::time::Duration::from_millis(10000));
    
    
    // going through the program
    let mut cycle_count = 0;
    let mut instruction_count: u64 = 0;
    let program_start = (now_nanoseconds(), std::time::Instant::now());
    let mut target = program_start.0;
    for _ in 0..1 {  // _000_000
        registers[14] = 0;
        loop {
            cycle_count += 1;
            instruction_count += 1;
            let pc = registers[14] as usize;
            debug_assert!(pc + 4 <= u16::MAX as usize);
            // grabbing 4 bytes (the maximum) at once to reduce cpu memory retrieval overhead
            let data = unsafe { instructions.get_unchecked(pc..pc + 4) };
            //println!("Data: {:x?}", data);
            let byte_code = data[0];
            let operands = &data[1..];
            
            // loading the instruction
            let byte_code_usize = byte_code as usize;
            let instruction_size = instruction_size_table[byte_code_usize];
            //println!("Size: {}; ops: {:x}/{:x?}", instruction_size, byte_code, &operands[..instruction_size as usize - 1]);
            //println!("Possible line: {:?}", tokens[instruction_refs[registers[14] as usize]]);
            //println!("Possible line count num: {}", instruction_refs[registers[14] as usize]);
            
            let mut interupt = Interrupt::Null;
            let mut jumped = false;
            
            //println!("vsync: {}", frame_signal);
            //if frame_signal {  println!("Frame!!!!");  }
            parse_instruction(
                byte_code,
                operands,
                &mut jumped,
                &mut interupt,
                &mut registers,
                &mut ram,
                &mut stack,
                &mut zero,
                &mut carry,
                &mut screen_buffer_pointer,
                &mut target,
                &mut cycle_count,
                &mut frame_signal as *mut bool
            );
            //println!("vsync: {}", frame_signal);
            
            match interupt {
                Interrupt::Halt => {
                    // syncing the timer on interrupts to ensure frame displaying and other actions can be time synced from a program
                    while now_nanoseconds() < target {
                        // busy wait....
                    }
                    
                    break;
                },
                Interrupt::Null => {},
                _ => {
                    // syncing the timer on interrupts to ensure frame displaying and other actions can be time synced from a program
                    while now_nanoseconds() < target {
                    // busy wait....
                    }
                }
            }
            
            // moving the counter to the next instruction (assuming no jumps happened)
            registers[14] += instruction_size as u16 * !jumped as u16;
            
            // incrementing the target time
            target += FRAME_TIME;
            
            debug_println!("Debug:  line/inst: {}/{:x}    registers: {:?}    ram (first 40bytes/20 addresses): {:?}     stack (first 40bytes/20 addresses): {:?}", registers[14], byte_code, registers, &ram[0..20], &stack[0..20]);
            //std::thread::sleep(std::time::Duration::from_micros(1));
            //std::thread::sleep(std::time::Duration::from_nanos(10));
        }
    }
    let program_end = now_nanoseconds();
    println!("Elapsed time: {}   avg. {}/{} over {} cycles and {} instructions", program_end - program_start.0, program_start.1.elapsed().as_nanos() as f64 / cycle_count as f64, (program_end - program_start.0) as f64 / cycle_count as f64, cycle_count, instruction_count);
    println!("Debug:  registers: {:?}    ram (first 40bytes/20 addresses): {:?}", registers, &ram[0..20]);
    
    sender.send(true).unwrap();
    thread.join().unwrap();  // by joining and waiting, this ensure there aren't any dangling references when this function goes out of scope
}

static FRAME_TIME: u64 = 59;  // 214 - 50;  targeting about 16.776 mhz which is the speed of one of the processors in the game boy advanced


use libc::{c_int, c_uint};
use crate::window::screen_thread;

#[repr(C)]
struct MachTimebaseInfoDataT {
    numer: c_uint,
    denom: c_uint,
}

unsafe extern "C" {
    fn mach_absolute_time() -> u64;
    fn mach_timebase_info(info: *mut MachTimebaseInfoDataT) -> c_int;
}

#[inline(always)]
fn get_timebase_info() -> MachTimebaseInfoDataT {
    unsafe {
        let mut info = MachTimebaseInfoDataT { numer: 0, denom: 0 };
        mach_timebase_info(&mut info);
        info
    }
}

#[inline(always)]
fn now_nanoseconds() -> u64 {
    unsafe {
        let time = mach_absolute_time();
        let info = get_timebase_info();
        time * (info.numer as u64) / (info.denom as u64)
    }
}


#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Interrupt {
    Halt   = 0,
    Cycles = 1,
    Buffer = 2,
    Null   = 3
}

#[inline(always)]
pub fn parse_instruction (
    byte_code: u8,
    operands : &[u8],
    jumped   : &mut bool,
    interrupt: &mut Interrupt,
    registers: &mut [u16; 23],  // 15 for general, the rest for alu and stuff
    ram      : &mut [u16; u16::MAX as usize+1],
    stack    : &mut [u16; u16::MAX as usize+1],
    zero     : &mut bool,
    carry    : &mut bool,
    screen_buffer_pointer: &mut usize,
    target   : &mut u64,
    i        : &mut u64,
    vsync    : *mut bool,
) {
    match byte_code {
        0x00 => {    },  // nop
        
        // alu
        0x11 => {  (registers[18], *carry) = registers[16].overflowing_add(registers[17])  },  // add
        0x12 => {  (registers[18], *zero) = overflow_add(registers[16].overflowing_add(registers[7]), *carry)  },  // addC
        0x13 => {  (registers[18], *carry, *zero) = is_zero(registers[16].overflowing_sub(registers[17]))  },  // sub
        0x14 => {  (registers[18], *zero) = overflow_sub(registers[16].overflowing_sub(registers[17]), *carry)  },  // subC
        0x15 => {  registers[18] = registers[16] | registers[17]  },  // or
        0x16 => {  registers[18] = registers[16] & registers[17]  },  // and
        0x17 => {  registers[18] = registers[16] ^ registers[17]  },  // xor
        0x18 => {  *zero = registers[16] > registers[17]  },  // cmpg
        0x19 => {  *zero = registers[16] < registers[17]  },  // cmpl
        0x1A => {  (registers[18], *carry) = registers[16].overflowing_add(1)  },  // inc
        0x1B => {  (registers[18], *carry, *zero) = is_zero(registers[16].overflowing_sub(1))  },  // dec
        0x1C => {  registers[18] = registers[16] << registers[17]  },  // left
        0x1D => {  registers[18] = registers[16] >> registers[17]  },  // right
        0x1E => {  registers[18] = registers[16].rotate_left(registers[17] as u32)  },  // rotl
        0x1F => {  registers[18] = registers[16].rotate_right(registers[17] as u32)  },  // rotr
        
        // basic memory
        0x21 => {  registers[(operands[0] >> 4) as usize] = registers[(operands[0] & 0x0F) as usize]  },  // mov
        0x22 => {  ram[(((operands[1] as u16) << 8) | (operands[2] as u16)) as usize] = registers[(operands[0] >> 4) as usize]  },  // str
        0x23 => {  registers[(operands[1] >> 4) as usize] = ram[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize]  },  // grab
        0x24 => {  registers[17] = registers[(operands[0] >> 4) as usize]  },  // ldrar
        0x25 => {  registers[16] = registers[(operands[0] >> 4) as usize]  },  // ldral
        0x26 => {  registers[(operands[0] >> 4) as usize] = registers[18]  },  // movreg
        0x27 => {  ram[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize] = registers[18]  },  // movram
        0x28 => {  *target += FRAME_TIME; *i+=1; registers[(operands[0] & 0x0F) as usize] = ram[registers[(operands[0] >> 4) as usize] as usize]  },  // ldrp
        0x29 => {  *target += FRAME_TIME; *i+=1; ram[registers[(operands[0] >> 4) as usize] as usize] = registers[(operands[0] & 0x0F) as usize]  },  // strp
        0x2A => {  registers[(operands[0] >> 4) as usize] = ((operands[1] as u16) << 8) | (operands[2] as u16)  },  // ldi
        0x2B => {  registers[17] = ((operands[0] as u16) << 8) | (operands[1] as u16)  },  // ldiar
        0x2C => {  registers[16] = ((operands[0] as u16) << 8) | (operands[1] as u16)  },  // ldial
        
        // i/o
        0x31 => {  registers[22] = 1; registers[20] = registers[(operands[0] >> 4) as usize]  },  // out    output = register, output flag = active
        0x32 => {  registers[(operands[0] >> 4) as usize] = registers[19]; registers[21] = 0;  },  // inp    register = input, input flag = inactive
        0x33 => {  if registers[21] > 0 {  *jumped = true; registers[13] = registers[13].overflowing_sub(1).0; registers[14] = stack[registers[13] as usize]  }},  // retio       // 19, 20, 21, 22    inp, out, inp flag, out flag
        0x34 => {  *zero = registers[21] > 0  },  // cmpio
        0x35 => {  registers[22] = 1  },  // setout
        
        /*
        1: register_rda, 2: register_rdb, 3: register_rdc, 4: register_rdd, 5: register_rde,
        6: register_rdf, 7: register_rdg, 8: register_rdh, 9: register_rdi, 10: register_rdj,
        11: register_rdk, 12: register_rdl, 13: register_stp, 14: register_pgc, 15: register_acc,
        16: register_alu_left, 17: register_alu_right, 18: register_alu_out,
        19: register_io_input, 20: register_io_output, 21: register_io_input_flag, 22: register_io_output_flag
        */
        
        // stack
        0x41 => {  *target += FRAME_TIME; *i+=1; stack[registers[13] as usize] = registers[(operands[0] >> 4) as usize]; registers[13] = registers[13].overflowing_add(1).0  },  // psh
        0x42 => {  *target += FRAME_TIME; *i+=1; registers[13] = registers[13].overflowing_sub(1).0; registers[(operands[0] >> 4) as usize] = stack[registers[13] as usize]  },  // pop
        0x43 => {  *target += FRAME_TIME * 2; *i+=2; registers[13] = registers[13].overflowing_sub(1).0; ram[registers[(operands[0] >> 4) as usize] as usize] = stack[registers[13] as usize]  },  // popptr
        0x44 => {  *target += FRAME_TIME * 2; *i+=2; stack[registers[13] as usize] = ram[registers[(operands[0] >> 4) as usize] as usize]; registers[13] = registers[13].overflowing_add(1).0  },  // pshptr
        0x45 => {  *target += FRAME_TIME; *i+=1; stack[registers[13] as usize] = ram[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize]; registers[13] = registers[13].overflowing_add(1).0  },  // pshrm
        0x46 => {  *target += FRAME_TIME; *i+=1; registers[13] = registers[13].overflowing_sub(1).0; ram[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize] = stack[registers[13] as usize]  },  // poprm
        0x47 => {  stack[(((operands[1] as u16) << 8) | (operands[2] as u16)) as usize] = registers[(operands[0] >> 4) as usize]  },  // strst
        0x48 => {  registers[(operands[2] >> 4) as usize] = stack[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize]  },  // ldrst
        0x49 => {  registers[(operands[0] >> 4) as usize] = registers[13]  },  // cpysp
        0x4A => {  registers[13] = registers[(operands[0] >> 4) as usize]  },  // setsp
        0x4B => {  *target += FRAME_TIME; *i+=1; stack[registers[13] as usize] = ((operands[0] as u16) << 8) | (operands[1] as u16); registers[13] = registers[13].overflowing_add(1).0  },  // pshcon
        0x4C => {  registers[13] = 0  },  // clrstk
        
        // branching
        0x51 => {  *jumped = true; registers[14] = ((operands[0] as u16) << 8) | (operands[1] as u16)  },  // jmp
        0x52 => {  if !*zero {  *jumped = true; registers[14] = ((operands[0] as u16) << 8) | (operands[1] as u16)  }},  // jnz
        0x53 => {  *target += FRAME_TIME; *i+=1; if *zero {  *jumped = true; registers[13] = registers[13].overflowing_sub(1).0; registers[14] = stack[registers[13] as usize]  }},  // retz
        0x54 => {  if *zero {  *jumped = true; registers[14] = ((operands[0] as u16) << 8) | (operands[1] as u16)  }},  // jiz
        0x55 => {  registers[14] = registers[(operands[0] >> 4) as usize]  },  // jmpr
        0x56 => {  if !*carry {  *jumped = true; registers[14] = ((operands[0] as u16) << 8) | (operands[1] as u16)  }},  // jnc
        0x57 => {  if *carry {  *jumped = true; registers[14] = ((operands[0] as u16) << 8) | (operands[1] as u16)  }},  // jic
        0x58 => {  *target += FRAME_TIME * 2; *i+=2; *jumped = true; registers[13] = registers[13].overflowing_sub(1).0; registers[14] = stack[registers[13] as usize]  },  // ret
        0x59 => {  *target += FRAME_TIME; *i+=1; stack[registers[13] as usize] = registers[14]; registers[13] = registers[13].overflowing_add(1).0  },  // pshpc
        0x5A => {  registers[(operands[0] >> 4) as usize] = registers[14]  },  // cpypc
        0x5B => {  *target += FRAME_TIME; *i+=1; *jumped = true; registers[14] = ram[registers[(operands[0] >> 4) as usize] as usize]  },  // jmpptr
        0x5C => {  *jumped = true; registers[14] = ram[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize]  },  // jmpad
        
        // interrupts
        0x61 => {  *interrupt = Interrupt::Halt  },  // halt
        0x62 => {  *interrupt = Interrupt::Buffer; *target += FRAME_TIME; *i+=1; *screen_buffer_pointer = registers[(operands[0] >> 4) as usize] as usize  },  // setbuf
        0x63 => {  *target += FRAME_TIME * 7; *i+=7; todo!()  },  // blkcpy
        0x64 => {  *target += FRAME_TIME * 7; *i+=7; todo!()  },  // sptcpy
        0x65 => {  *interrupt = Interrupt::Cycles; registers[(operands[0] >> 4) as usize] = (*i & 0x0000_0000_0000_FFFF) as u16; /*casting to prevent overflow; using an add and waiting for greater should be safe even if an overflow is cast down*/  },  // cycles
        0x66 => unsafe {  *zero = false; if *vsync { *vsync = false; *zero = true; }  },  // sync
        
        // advanced memory
        0x71 => {  registers[(operands[0] >> 4) as usize] = 0x0000 | *zero as u16 | ((*carry as u16) << 1)  },  // flags
        0x72 => {  registers[(operands[0] >> 4) as usize] = 0  },  // clr
        0x73 => {  ram[(((operands[0] as u16) << 8) | (operands[1] as u16)) as usize] = 0  },  // clrmem
        0x74 => {  *zero = registers[16] == registers[17]  },  // cmpeq
        0x75 => {  *zero = registers[16] == 0  },  // cmp
        0x76 => {  registers[(operands[0] >> 4) as usize] = *zero as u16  },  // flgzro
        0x77 => {  registers[(operands[0] >> 4) as usize] = *carry as u16  },  // flgcry
        0x78 => {  registers[18] = registers[16]* *zero as u16 + registers[17]* (1- *zero as u16) },  // memcmp   aka let value = {if condition -> 1 else -> 2}
        0x79 => {  let reg_add = (operands[0] >> 4) as usize; registers[reg_add] = registers[reg_add]* !*zero as u16 + registers[18]* *zero as u16  },  // movz
        0x7A => {  let reg_add = (operands[0] >> 4) as usize; registers[reg_add] = registers[reg_add]* *zero as u16 + registers[18]* !*zero as u16  },  // movnz
        0x7B => {  let reg_add = (operands[0] >> 4) as usize; registers[reg_add] = registers[reg_add]* !*carry as u16 + registers[18]* *carry as u16  },  // movc
        0x7C => {  let reg_add = (operands[0] >> 4) as usize; registers[reg_add] = registers[reg_add]* *carry as u16 + registers[18]* !*carry as u16  },  // movnc
        0x7D => {  *target += FRAME_TIME * 4; *i+=4; unsafe {
                // copying a slice [x1..x2] of ram to another section starting at x3 going to < x3+len
                // as long as the programmer didn't write an assembly script that accesses out of bounds memory, there should be no issues
                // kinda a big foot-gun, but whatever, it's assembly after all so nothing's safe and this should ensure good performance
                std::ptr::copy(
                    ram.as_ptr().offset(0),  // the pointer to the source in ram
                    ram.as_mut_ptr().offset(0),  // the pointer to the destination in ram
                    operands[1] as usize  // the specified size of the chunk to copy
                );
            }
        },  // memcpy
        0x7E => {  *target += FRAME_TIME; *i+=1; todo!()  },  // ramswp
        
        0x81 => {(registers[18], *carry) = registers[16].overflowing_mul(registers[17])},  // mult
        0x82 => {*target += FRAME_TIME; *i+=1; registers[18] = registers[16].checked_div(registers[17]).unwrap_or(0)},  // div
        
        v => {  panic!("Invalid Instruction: {:x}", v)  }  // nop (but not an official instruction
    }
}
#[inline(always)]
pub const fn overflow_add ((value, overflow): (u16, bool), carry: bool) -> (u16, bool) {
    let result = value.overflowing_add(carry as u16);
    (result.0, result.1 || overflow)
}
#[inline(always)]
pub const fn overflow_sub ((value, overflow): (u16, bool), carry: bool) -> (u16, bool) {
    let result = value.overflowing_sub(carry as u16);
    (result.0, result.1 || overflow)
}
#[inline(always)]
pub const fn is_zero (value: (u16, bool)) -> (u16, bool, bool) {
    (value.0, value.1, value.0==0)
}
