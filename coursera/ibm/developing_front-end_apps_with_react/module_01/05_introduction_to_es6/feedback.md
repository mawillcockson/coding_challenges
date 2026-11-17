## [Introduction to ES6][coursera link]

> There is only one line of code, but because the code returns a value, it must be in curly brackets:
>
>     const oneParamArrowFunction = name => { return "hello" + name };

I disagree with the statement: `param => return_value` is a perfectly valid arrow function. I'm not sure how to read [the ECMAScript syntax definition for an arrow function][ecma-arrow], but I know from experience that this function can absolutely be written as:

```javascript
const oneParamArrowFunction = name => "hello" + name;
```

However, passed `"Matthew"`, this would produce `helloMatthew`. Let's fix this. And, while we're at it: string concatenation is so ES5. Let's use a template literal. And add types!

```typescript
const oneParamArrowFunction = (name: string): string => `hello ${name}`;
```

<p class="small-note"><a href="https://www.typescriptlang.org/play/v2/?ts=next#code/v2/N4IghgxgLglgbgUwGIwDYJALhAegO4D2ATgNYDOADpAjmURDjAHYAmCAHgHRRkgA0IAGZoEvTKHzFyVCDR4QCTYQHNOAKzKKsIYAB0mAAgO6QCgLYURRAPIVYisicwG9ho8ZBkoRGNCcHvAFcEPn13DyYCAEkLVF8YKABBJgBPfyCQsPcTLx9oADlA1FQAYQALBAhydKJg0LcjHO9fKCRApmgYRQAVFIpRGrqsxs9m6AAFIgJ+oigUqKYEmDA4gC8weyZBzIaPXJaAIWYWEpXURIoKVDSsANqd8JNImKv4qG6ymEdbjPrHkGesTeACUEFBAkQmN9nL9hh4zgQ8ABVJhEBCQMpgABG6BKBDY-kEKzID2y4GKiJRgRJLAAMtiEKhoQYiUzSSMoGAiMowf4TKImBwoCY-mSNOw+SA0ZBhfw4SYzPiighJQKhSL5QCCABRIhTIjdWodDZdLY-e6ikYUNFQObbLIAX30Dv4uEIpEo1Fo9EYrA43DEpgcUAMigQ4y5YDMiT1iLaHU2BgAvAYABRMKMIZz7JjKACU2eaueTAD4DAADCoUgwAEmAGbMCAd5YA3Pp9AooQR0JxUARlKmwxGiFGY1M8PHOopUyYALIbKAVPAmPN5lsgB0CEnoKdMLCgCgEMhLRR41CBMx7zAARgEh+Pm1pzAQhTMWIQRCwt88jMqmwAypysxnheV7ftuf6moBXJQE+gqvu+n43puICIEQx5aJgABMDpAA">Typescriptlang.org Playground link</a></p>

Much better!

The other functions on this same slide could use the same touchups.

[coursera link]: <https://www.coursera.org/learn/developing-frontend-apps-with-react/lecture/2jKWN/introduction-to-es6>
