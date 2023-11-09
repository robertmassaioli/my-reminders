<!DOCTYPE html>
<html lang="en">
   <head profile="http://www.w3.org/2005/10/profile">
      <link rel="icon" type="image/ico" href="/static/${resourcesVersion}/images/favicon.v3.ico" />

      <apply template="headers/requirejs" />
      <apply template="headers/aui" />
      <apply template="headers/aui-experimental" />
      <js src="/static/${resourcesVersion}/js/app/doc-page.js" />
      <script>require(['app/doc-page']);</script>
   </head>
   <body>
      <header id="header" role="banner">
         <nav class="aui-header aui-dropdown2-trigger-group" role="navigation">
            <div class="aui-header-primary">
               <h1 id="logo" class="aui-header-logo aui-header-logo-custom"><a href="/"><span class="aui-header-logo-text">My Reminders</span></a></h1>
               <ul class="aui-nav">
                  <!-- You can also use a split button in this location, if more than one primary action is required. -->
                  <li><a class="aui-button aui-button-primary" href="/redirect/install">Install plugin</a></li>
               </ul>
            </div>
            <div class="aui-header-secondary">
               <ul class="aui-nav">
                  <li><a href="/redirect/help" aria-owns="dropdown2-header7" aria-haspopup="true" class="aui-dropdown2-trigger-arrowless aui-dropdown2-trigger" aria-controls="dropdown2-header7"><span class="aui-icon aui-icon-small aui-iconfont-help">Help</span></a>
                  <div class="aui-dropdown2 aui-style-default aui-dropdown2-in-header" id="dropdown2-header7" style="display: none; top: 40px; min-width: 160px; left: 1213px; " aria-hidden="true">
                     <div class="aui-dropdown2-section">
                        <ul>
                           <li><a href="/redirect/raise-issue">Report a bug</a></li>
                           <li><a href="/docs/about">About</a></li>
                        </ul>
                     </div>
                  </div>
                  </li>
               </ul>
            </div>
         </nav>
      </header>

      <div class="aui-page-panel">
         <div class="aui-page-panel-inner">
            <div class="aui-page-panel-nav">
               <nav class="aui-navgroup aui-navgroup-vertical">
                  <div class="aui-navgroup-inner">
                  <ul class="aui-nav">
                  <li><a href="/docs/home">Welcome</a></li>
                  </ul>
                  <div class="aui-nav-heading"><strong>More</strong></div>
                  <ul class="aui-nav">
                  <li><a href="/docs/about">About</a></li>
                  <li><a href="/docs/faq">FAQ</a></li>
                  <li><a href="/redirect/raise-issue">Report a bug</a></li>
                  </ul>
                  </div>
               </nav>
            </div>

            <div class="hidden" id="page-markdown-content"><includeFile file="resources/docs/${fileName}.markdown" /></div>
            <section id="page-html-content" class="aui-page-panel-content">
               <p>Loading page content...</p>
            </section>
         </div>
      </div>

      <footer id="footer" role="contentinfo">
         <section class="footer-body">
            <ul>
               <li><a target="_blank" href="https://developer.atlassian.com/static/connect/docs/latest/index.html">Atlassian Connect</a></li>
               <li><a target="_blank" href="/atlassian-connect.json">Add-on descriptor</a></li>
            </ul>
            <div id="footer-logo"><a href="http://www.atlassian.com/" target="_blank">Atlassian</a></div>
         </section>
      </footer>
   </body>
</html>
