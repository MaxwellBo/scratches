interface Has2DCoords {
    x: number
    y: number
}


function nudge<T extends Has2DCoords>(point: T): T {
    return { ...point, x: point.x + 1 }
}

const hmm = { x: 1, z: 3 }

console.log(nudge(hmm));