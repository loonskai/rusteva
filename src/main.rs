mod eva;
mod environment;
mod error;
struct MyString<T> {
    value: T
}

impl MyString<&str> {
    fn len(&self) -> i32 {
        let mut length = 0;
        self.value.chars().for_each(|_| {
            length += 1;
        });
        return length;
    }
}

impl<T> From<T> for MyString<T> {
    fn from(value: T) -> Self {
        return MyString { value: value };
    }
}

fn main() {
    let x = i32::abs(2);
    let s = MyString::from("Hello");
    let s_len1 = s.len();
    println!("{s_len1}");
}


