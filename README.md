
# scat

`scat` is a *password scatterer*. It allows the generation of unique passwords for each service,
website, email address or account you might have, all from a single password and a secure code you keep.

## Motivation

Nowadays, accounts for many services such as Facebook, Twitter, Reddit, Google, Amazon, your bank account, etc. are needed.
In a perfect world, all those accounts would have different passwords, so that, if someone gets to know, let's say, your Facebook password,
they don't gain access to your bank account and your money as well. But, on the other hand, who would like to come up with dozens and dozens of different passwords? Moreover, if you ever lose access you key chain, how can you recover all your passwords?

`scat` is the solution to this problem. It allows you to safely generate for each website or service you suscribe to a unique password. All you have to do is remember a single, as strong as possible, password and securely keep a long random code.

Given the same information, `scat` will always generate the same password, so if you were to completely lose your entire key chain,
you can always retrieve them.
Passwords generated by `scat` are very secure and independant of each others. If by misfortune one of the generated password is compromised, all other passwords are still safe, and so is the password you used to generate them.

## How to use 

`scat` is a secure password generator. It does not replace your favorite secure key chain, but it should be seen as a completement to it. Use `scat` to generate a different password for each of the service you suscribe to, and use your key chain (such as [pwsafe][pwsafe] for instance) to keep the generated passwords.


## Example

To use `scat`, simply call it specifying which key, or service, it must generate a password for.
Then, simply enter your password (which is, in this example, `pony1234`, and code, `AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9`):

```
> scat -c -S "github"
Password:
Confirm:
Code: AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9
Generated password:
k2'8n?QXwmptbJ7D44
```

> Note that `pony1234` is **not** your github password. It's the original password you use to generate other passwords.
> **Never share it, or use it as a password for anything else than `scat`.**

Let's say that you now want to generate a Facebook password:

```
> scat -c -S "facebook"
Password: 
Confirm:
Code: AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9
Generated password:
{g6e2hsKjh#Ra*\ks(
```
The password generated for Facebook is completely different from the one it generated for GitHub.


Imagine now that your are on an other computer, with no access to your key chain, and you would like to login to Facebook (just for 5 minutes).
To your great despair, there is no way you can remember your obscure password!
However, as you always keep your code with you and `scat` is fully deterministic, you can simply call it once more, to generate once again the exact same password, this time from another computer.

```
> scat -c -S "facebook"
Password: 
Confirm:
Code: AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9
Generated password:
{g6e2hsKjh#Ra*\ks(
```

> **CAUTION** `scat` is not an excuse to use a weak password such as `pony1234`. Always use a strong password (that you can remember) with `scat`.

## Generating a suitable code

There are many ways to generate and store a suitable code. For instance, you can use a key such as [those provided by yubico][yubico-key], which can [generate and store static keys][yubico-static]. Be sure to use the maximal key size!

However, if you do not want to invest in a key, there exists secure solutions at your disposition that won't code you anything.
For this, all you need is two dice that you can differentiate (one red and the other black for instance) and a small piece of paper (for instance of credit card format).
For 25 consecutive rounds, throw the two dice, lookup the result in the table below and report it on the piece of paper.
You may want to put a dash between the various results, or any other separator, to increase readability.

Die 1 | Die 2 | Result |   | Die 1 | Die 2 | Result |   | Die 1 | Die 2 | Result
:---: | :---: | :----: | - | :---: | :---: | :----: | - | :---: | :---: | :----:
1     | 1     | 1      |   | 3     | 1     | C      |   | 5     | 1     | O
1     | 2     | 2      |   | 3     | 2     | D      |   | 5     | 2     | P
1     | 3     | 3      |   | 3     | 3     | E      |   | 5     | 3     | Q
1     | 4     | 4      |   | 3     | 4     | F      |   | 5     | 4     | R
1     | 5     | 5      |   | 3     | 5     | G      |   | 5     | 5     | S
1     | 6     | 6      |   | 3     | 6     | H      |   | 5     | 6     | T
2     | 1     | 7      |   | 4     | 1     | I      |   | 6     | 1     | U
2     | 2     | 8      |   | 4     | 2     | J      |   | 6     | 2     | V
2     | 3     | 9      |   | 4     | 3     | K      |   | 6     | 3     | W
2     | 4     | 0      |   | 4     | 4     | L      |   | 6     | 4     | X
2     | 5     | A      |   | 4     | 5     | M      |   | 6     | 5     | Y
2     | 6     | B      |   | 4     | 6     | N      |   | 6     | 6     | Z

Yet another solution would be to get a set of cards (a deck of 32 cards is sufficient). Shuffle it for some time, and then shuffle it again, just to be sure. Get a piece of paper and note down the order of the cards, using an abreviation for each card. For instance, you can use `KS` for for the King of Spades and `10H` for the 10 of Heart. You can now either keep the piece of paper securely with you, or keep your set of cards in the same order always in your bag and starting over when you need the code!

### What if I lose my code?

If you still have access to the key chain on your computer, no big deal, just generate a new random code and change all your passwords when convenient. If you have ever lose your code, the only consequence is that you can not re-generate a previously generated password.

Now, if you lose access to both your key chain and your code, all your passwords are definitely lost. Sorry.

## Security

By default, `scat` will generate password of length 18, using a mix of lower case letters, upper case letters, digits and various ascii symbols.
This leads to a password entropy of about 115 bits. Meaning that an attacker
knowing which schema you used and able to test a billion password per second would have to wait approximately 50 million times *the age of the universe* to 
guess your password correctly. So it's pretty safe.

Now, let's imagine for a second that an attacker gets to know one of your generated password.
This is pretty bad, but not as bad as having a single password. Imagine for a second the disaster if your attacker could directly access all your accounts!
Knowing a single generated password won't help your attacker much, it is still practically infeasable for him to get to your original password, so all of your other accounts are safe!

## Password schemas

As we have just seen, `scat` generates by default password of length 18. But, it also provides other schemas!

If you want, for some reason, an easily rememberable passphrase, for let's say Facebook, you can use the schema named `diceware`,
which will output 5 words out of the 7776 words of [the Diceware list][diceware].

```
> scat -c -S "facebook" -s diceware
Password: 
Confirm:
Code: AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9
Generated password:
101 dry whoa foil barb
```

Or, if you prefer Pokémons:

```
> scat -c -S "facebook" -s pokemons
Password:
Confirm:
Generated password:
Snorlax 5, Weedle 35, Raichu 27, Alakazam 99
```

### Summary of available schemas

Name | Comments | Entropy (in bits)
:--- | :------- | :----------------
safe | A mix of 18 ascii symbols. (default) | 115
alpha | A mix of 18 alphanumeric symbols. | 104
parano | 78 ascii characters. | 512
pokemons | 4 classic Pokémons, each with a level up to 100. | 55
diceware | 5 words out of the [diceware's list][diceware]. | 65
pinXX | `XX` digits. (useful for PIN codes) | `3.3 * XX`

## How it works

Under the hood, `scat` will use `Scrypt`, a password-based key derivation function, to generate a huge integer seed.
This integer seed will be consumed by `scat` to generate deterministically a new password from a schema.

## Installing

For now, `scat` is only distributed as a Haskell source code. 

1. If you do not have Haskell and Cabal installed, please visit [the Haskell website and download the Haskell platform][haskell-platform].

2. Once this is done, download the `scat` code and place yourself in the root directory of the project (in which you should find a file named `snap.cabal`).

3. Run the command `cabal configure && cabal install` to install `scat`.

4. Enjoy!

## Contributing

Feel free to contribute to this project! If you have a brilliant idea to make this project better, just say so!
If you lack ideas but would like to participate anyway, you can also find here a list of things to do!

### Things to do

1. Create other schemas.

2. Let the user specify a size for some schemas. (By specify another command line option).

3. Compatible port in mainstream programming languages.

4. Some other improvements you might want!

### Contributors

Name | Contributions
:--- | :------------
Romain Edelmann | Initial work on the project.

[yubico-key]: http://www.yubico.com/products/yubikey-hardware/
[yubico-static]: http://www.yubico.com/products/services-software/personalization-tools/static-password/
[pwsafe]: http://nsd.dyndns.org/pwsafe/
[diceware]: http://world.std.com/~reinhold/diceware.html
[haskell-platform]: http://www.haskell.org/platform/