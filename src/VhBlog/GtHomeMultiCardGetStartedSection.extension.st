Extension { #name : #GtHomeMultiCardGetStartedSection }

{ #category : #'*VhBlog' }
GtHomeMultiCardGetStartedSection >> blogCard [
	<gtSectionCard>
	<gtExample>
	<noTest>
	^ GtHomeCard lepiterBaseWithTableOfContents
		title: 'Veit’s Blog';
		description: 'Read and write blog posts';
		priority: 10;
		databaseName: 'hellerve/blog/lepiter';
		readMoreToolbar;
		details: [ :db | 'Veit’s blog' ] yourself
]
