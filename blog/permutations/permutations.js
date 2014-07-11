/*
 * This is a JavaScript Scratchpad.
 *
 * Enter some JavaScript, then Right Click or choose from the Execute Menu:
 * 1. Run to evaluate the selected text (Ctrl+R),
 * 2. Inspect to bring up an Object Inspector on the result (Ctrl+I), or,
 * 3. Display to insert the result in a comment after the selection. (Ctrl+L)
 */

var initial_deck =
    [ 'A'
    , '2'
    , '3'
    , '4'
    , '5'
    ]

function step1(decks) {
    var d1 = decks[0]
    var d2 = decks[1]
    
    
}

function step2(decks) {
    var d1 = decks[0]
    
    
}

function step(decks) {
    var deck = args[0]
    var pile = args[1]
    
    if (deck.length == 0) {
        return [deck, pile]
    } else if (deck.length == 1) {
        var x = deck[0]
        return [deck.slice(1), pile.unshift(x)]
    } else {
        var x = deck[0]
        var y = deck[1]
        return [deck.push(y), pile.unshift(x)]
    }
}

function update(decks) {
    var new_decks = step(decks);
}

update([initial_deck, []])