# Neural Network/Genetic Algorithm in Common Lisp

This was a project I worked on in high school in the early 2000s. I wanted to learn
about neural networks, and while Googling around I learned that the lingua franca of AI
(this is before the term machine learning existed) was Common Lisp, so I got a few books
on CL and taught myself.

Then I discovered Neural Networks and set about to learn those. I couldn't find anything
succinct about how backpropagation worked, it was all a bunch of terse calculus I didn't understand.
But I did discover another training method called Genetic Algorithms and was able to figure them
out from a bunch of whitepapers.

So I implemented a basic genetic algorithm for evolving neural networks. The code is terribly inefficient,
but it was a fun exercise. I later reimplemented it in Python, C++, Java, and C as a way to learn those
languages. It was still too slow to do anything meaningful on a single CPU so I spent some time trying
to distribute the GA to run across a cluster of machines (what we called a Beowolf cluster back then),
but by the time I was getting something meaningful working, GP-GPU computing showed up and optimizing for
CPU performance was suddenly moot.

I eventually lost interest in the subject after working on it for most of a decade, and of course that was right
when the field of Machine Learning suddenly became hugely popular, but I had moved on to other subjects. I still
wonder where I might have wound up if I'd kept up with it.
