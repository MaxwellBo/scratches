function f<T extends boolean>(x: T): T extends true ? string : number {
    if (x) {
        return "it's true"
    } else {
        return 0
    }
}

let x = f(true)


interface Foo {
    a: string
    b: boolean
    c: number
}

type PickResult = Pick<Foo, 'a' | 'b'>
// { 
//     a: string; 
//     b: boolean; 
// }

type RecordResult = Record<'a' | 'b' | 'c', string>
// {
//     a: string;
//     b: string;
//     c: string;
// }