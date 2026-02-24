---
title: "Unstar everything, unfollow everyone (sorry)"
date: 2023-10-02
---

A few weeks ago, [GitHub changed its home feed](https://github.com/orgs/community/discussions/65343). While I don’t want to comment on its utility generally, it’s been somewhat controversial, and for me personally, my home feed has become useless enough that I’ve overcome my usual inertia about changes like these and decided to take some action to make my home feed more useful.

So, what do I want in a home feed? Broadly speaking, I want two things: I want to see activity in repositories I’m watching, and see if and how people interact with my repositories.

Unfortunately, the only way that I saw to reclaim some sort of utility in my feed was to unstar all the repositories that I had starred, and to unfollow everyone I’ve been following. That’s a bit of a bummer, because it takes the joy out of the GitHub experience; a part of following and starring is just to give props to a person or project. Mostly what I want to express with either of those action is: “Hey, that’s pretty cool! You’re doing a great job!” It’s nice to be complimented.

So I want to apologize to all the people I’ve basically shafted in that way and robbed of their well-deserved compliments. It’s a fundamentally selfish thing to do, but it became somewhat important to me.

Phew. Now that that is out of the way, let’s dive into how I did it, shall we? After all, it might be a nice little exercise in scripting an API from Glamorous Toolkit, my weapon of choice. After all, I had around 1,400 starred repositories and followed over 170 people, and I wasn’t going to click through all of that manually.

## A dive into the GitHub API

GitHub has [a reasonable, well-documented REST API](https://docs.github.com/en/rest?apiVersion=2022-11-28) that I’ve used before and decided to use again. The first thing I always forget is how to generate an API token (hint: [this is how you do it](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-personal-access-token-classic)). They have two flavors, ”fine-grained” and “classic”, and as an aging developer and overall purist of course I chose the classic option (and definitely not because I couldn’t for the life of me figure out what scopes I needed to select in the fine-grained version).

Anyway, we end up with a token in our clipboard that we can copy into a variable..

```smalltalk
aToken := Clipboard clipboardText
```

Next, we need to figure out how to get to our starred repositories. The `/user` endpoint holds all information about the currently authenticated user, and it has a sub-endpoint `/user/starred` that does exactly what we need.

```smalltalk
repos := STONJSON fromString: (ZnClient new
	url: 'https://api.github.com/user/starred';
	setBearerAuthentication: aToken;
	get)
```

We can then go through these repositories and delete them by using the information from the list elements.

```smalltalk
repos
	do: [ :aRepo | 
		ZnClient new
			url: 'https://api.github.com/user/starred/' , (aRepo at: 'owner' at: 'login') , '/'
					, (aRepo at: 'name');
			setBearerAuthentication: aToken;
			delete ]
```

Unfortunately, I had enough repositories starred that pagination kicked in. Too lazy to figure out how to paginate, I just decided to call the list endpoint until I got an empty list or `null`, at which point I assumed I was done. The resulting code is an awful `whileTrue:` loop, which is only excusable because we’re scripting, baby:

```smalltalk
[ repos := STONJSON
		fromString: (ZnClient new
				url: 'https://api.github.com/user/starred';
				setBearerAuthentication: aToken;
				get).
repos isEmptyOrNil not ]
	whileTrue: [ repos
			do: [ :aRepo | 
				ZnClient new
					url: 'https://api.github.com/user/starred/' , (aRepo at: 'owner' at: 'login') , '/'
							, (aRepo at: 'name');
					setBearerAuthentication: aToken;
					delete ] ]
```

We now know the drill for people we follow. Get the list, delete, iterate until empty. How do we get the list? This endpoint I just guessed by looking at the schema above, and figured that `/user/following` might work.

```smalltalk
following := STONJSON fromString: (ZnClient new
	url: 'https://api.github.com/user/following';
	setBearerAuthentication: aToken;
	get)
```

That one indeed does the trick! Now, let’s delete, and this time jump right into the `whileTrue:` version. Two things: because I am the follower, I called the model I’m getting `aLeader`—I might as well have called it `aUser`, but I felt cute. As for the endpoint for deletion, I guessed, and guessed right again: just append the name of the user you want to unfollow to the endpoint above, and send a `DELETE` request.

```smalltalk
[ following := STONJSON
		fromString: (ZnClient new
				url: 'https://api.github.com/user/following';
				setBearerAuthentication: aToken;
				get).
following isEmptyOrNil not ]
	whileTrue: [ following
			do: [ :aLeader | 
				ZnClient new
					url: 'https://api.github.com/user/following/' , (aLeader at: 'login');
					setBearerAuthentication: aToken;
					delete ] ]
```

Aaaaaaand I’m all alone in my feed. Whether that sentence is filled with dread or bliss I’ll leave up to you.

## Fin

Again, to all the people I’ve followed and whose repositories I’ve starred: I’m sorry, I still love you! You will always have that imaginary internet point in my heart.

And to all of you who might want to do the same and I might be the collateral damage this time: No hard feelings, you may even use this blog post as inspiration!
