## Changes between rss-utils 0.0.9 and 0.0.10 (Nov 18th, 2017)
### Bumped to use data.xml 0.2.0-alpha3 (w/ hotfix)
* This version allows the use of XML namespaces
* Inline monkey-patch to deal with a bug that throws exception on up xmlns and
  xml ns definitions even when they're correct

### Exception handling for -get-body
* I have at least one feed that's not playing ball with the way I'm fetching the
  feed body, so at least it tells me now
