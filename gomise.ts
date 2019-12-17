interface Ok<A> {
    isOk: true
    value: A
}

interface Err<E> {
    isOk: false
    error: E
}

type Result<E, A> = Ok<A> | Err<E>

function err<E>(error: E): Err<E> {
    return {
        isOk: false,
        error
    }
}

function ok<A>(value: A): Ok<A> {
    return {
        isOk: true,
        value
    }
}

// assert<Equal<1 | Top, Top>>()
// assert<Equal<1 | Bottom, 1>>() 

// assert<Equal<1 & Top, 1>>()
// assert<Equal<1 & Bottom, Bottom>>()

type Top = {} // ⊤ everything can be assigned to top
type Bottom = never // ⊥ can be assigned to everything


type NoErrors = Bottom
type AllErrors = Top

type NoContext = Top
type AllContext = Bottom

type Task<E, A> = Prog<NoContext, E, A>

function mapResult<E, A, B>(f: (a: A) => B, result: Result<E, A>): Result<E, B> {
    if (result.isOk) {
        return ok(f(result.value));
    } else {
        return result as Result<E, B>;
    }
}

class Prog<R, E, A> {

    static succeed<A>(value: A): Prog<never, never, A> {
        return this.unsafeFromPromise(Promise.resolve(value));
    }

    static fail<E>(error: E): Prog<{}, E, {}> {
        return this.unsafeFromPromise(Promise.reject(error));
    }

    static fromPromise<A>(promise: Promise<A>): Prog<NoContext, NoErrors, A> {
        return new Prog((r: never) => promise);
    }

    static unsafeFromPromise<E, A>(promise: Promise<A>): Prog<never, E, A> {
        return new Prog((r: never) => promise);
    }

    static toPromise<E, A>(zio: Task<E, A>): Promise<Result<E, A>> {
        return zio.attempt().underlying(null as never);
    }

    static ask<R>(): Prog<R, never, R> {
        return new Prog((r: R) => Promise.resolve(r));
    }

    static access<R, A>(f: (r: R) => A): Prog<R, never, A> {
        return new Prog((r: R) => Promise.resolve(f(r)));
    }

    static accessM<R = AllContext, S = AllContext, E = AllErrors, A = unknown>(f: (r: R) => Prog<S, E, A>): Prog<R & S, E, A> {
        return this.ask<R>().then<S, E, A>(f);
    }

    constructor(private underlying: (context: R) => Promise<A>) {
    }

    map<B>(f: (a: A) => B): Prog<R, E, B> {
        return new Prog((r: R) => this.underlying(r).then(f));
    }

    then<S, F, B>(f: (x: A) => Prog<S, F, B>): Prog<R & S, E | F, B> {
        return new Prog((r: R & S) =>
            this.underlying(r).then((a: A) => f(a).underlying(r))
        )
    }
    
    provide(context: R): Task<E, A> {
        return new Prog((r: never) => this.underlying(context));
    }

    attempt(): Prog<R, never, Result<E, A>> {
        return new Prog((r: R) => this.underlying(r).then(ok).catch((e: E) => err(e)));
    }
}


interface NumberProvider {
    five: Prog<NoContext, NoErrors, number>
}

const ctx: NumberProvider = {
    five: Prog.succeed(5)
}

const five = () => Prog.accessM<NumberProvider, NoContext, NoErrors, number>((f: NumberProvider) => f.five)

const x: Prog<NoContext, NoErrors, number> = Prog.succeed(5)

x.provide({ x: 5, z: 6, x: 7  });

const program = five().then(x => five().map(y => x + y))

Prog.toPromise(program.provide(ctx)).then(console.log);
