
pub static SHOW_CURSOR: &str = "\x1b[?25h";
pub static HIDE_CURSOR: &str = "\x1b[?25l";


// Wrapper struct for the raw pointer
#[derive(Clone, Copy)]
pub(crate) struct RawPtr<T>(*mut T);

// Mark RawPtr as Send and Sync
unsafe impl<T> Send for RawPtr<T> {}
unsafe impl<T> Sync for RawPtr<T> {}

impl<T> RawPtr<T> {
    pub(crate) fn new(ptr: *mut T) -> Self {
        RawPtr(ptr)
    }
    
    pub(crate) fn get(&self) -> *mut T {
        self.0
    }
}


pub fn screen_thread (ram_pointer: RawPtr<u16>, buffer_pointer: RawPtr<usize>, frame_start_signal_pointer: RawPtr<bool>, receiver: crossbeam::channel::Receiver<bool>) {
    print!("{}", HIDE_CURSOR);
    while receiver.try_recv().is_err() {
        //std::thread::sleep(std::time::Duration::from_millis(100));
        // screen specs:
        // 128 x 80 resolution
        // 32 blacking columns, 8 rows   for 160 x 88
        // do the math now........ jk way too lazy rn
        // 2.5 seconds per frame (to meet 24 hertz)
        // 28.4090909ms per row; 2.27272727sec rendering, 0.227272727sec vert blanking
        // 2.95928267 micro sec per pixel; 22.7272727ms of rendering per row and 5.68181818ms of blanking
        // have fun with the super precise timing on all of this.........   good news, std should be precise enough?
        
        // starting the frame
        let frame_start = std::time::Instant::now();  // making sure the overall frame is precise despite tiny errors along the way
        unsafe {  *frame_start_signal_pointer.get() = true;  }
        
        // the vert blanking time wil just happen in the final wait for the whole frame (that should reduce tiny errors adding up to larger errors over time)
        for row in 0..80 {
            let start = std::time::Instant::now();  // timing the pixel to ensure accuracy
            
            // running the horizontal scan
            for collumn in 0..160 {
                let collumn_start = std::time::Instant::now();
                if collumn < 128 {
                    // rendering the pixel
                    // getting the color from ram
                    let pixel_index = collumn + row * 128 + unsafe {  *buffer_pointer.get()  };
                    let color = unsafe {  // getting the color based on the index just calculated
                        *ram_pointer.get().offset(pixel_index as isize)
                    };
                    
                    // rgb: 5, 6, 5
                    // rrrrr gggggg bbbbb
                    // seperating each color out of the mix
                    let red   = ((color >> 11) as f64 * 8.22580645161) as usize;
                    let green = (((color >> 5) & 0b0000000000111111) as f64 * 4.04761904762) as usize;
                    let blue  = ((color & 0b0000000000011111) as f64 * 8.22580645161) as usize;
                    
                    // printing the color to the screen based on the current position
                    // hopefully a regular print is fast enough.... buffering would remove the cool screen tearing and artifacts that I want
                    // "/x1b[38;2;{};{};{}", rn, gn, bn       /x1b[{line};{collumn}H
                    // moving the cursor, than setting the color, than rendering the square
                    // \x1b[0;{};{};{}m
                    print!("\x1b[{};{}H\x1b[0;48;2;{};{};{}m   ", row + 2, collumn*3 + 3, red, green, blue);  // 2 spaces that have a background color should work?
                }
                while collumn_start.elapsed().as_nanos() < 2959 {}
            }
            
            while start.elapsed().as_nanos() < 473484 {}
        }
        // removing 0.1 of a sec to hopefully remove any tiny errors that might accumulate over time
        while frame_start.elapsed().as_nanos() < 41666666 {}  // ensureing the total frame time aligns correctly incase of tiny errors along the way
        //let frames = 1. / frame_start.elapsed().as_secs_f64();
        //println!("\x1b[0m\x1b[83;1HElapsed frame rate: {}", frames);
        //println!("{}", unsafe{*buffer_pointer.get()});
    }
    
    print!("{}", SHOW_CURSOR);
}


