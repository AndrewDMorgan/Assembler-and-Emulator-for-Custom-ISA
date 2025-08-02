use std::io::Read;
use crate::window;

pub(crate) fn keyboard_handler (receiver: crossbeam::channel::Receiver<bool>, registers: window::RawPtr<u16>) {
    let mut stdin = std::io::stdin();
    while receiver.try_recv().is_err() {
        let mut local_buffer = [0; 12];
        let result = stdin.read(&mut local_buffer);
        if let Ok(n) = result {
            //println!("Keycode: {:x}, buffer: {:x?}", n, local_buffer);
            if local_buffer[0] == 0x51 {  // safety release to prevent a runaway.....
                // capital Q
                crossterm::terminal::disable_raw_mode().unwrap();
                return;
            }
            unsafe {
                // updating the i/o port
                *registers.get().offset(19) = local_buffer[0] as u16;
                *registers.get().offset(21) = 1;
            }
        }
    }
}

