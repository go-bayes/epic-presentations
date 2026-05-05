// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="index.html">Welcome</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Course Information</li><li class="chapter-item expanded "><a href="schedule.html"><strong aria-hidden="true">1.</strong> Schedule</a></li><li class="chapter-item expanded "><a href="assessments.html"><strong aria-hidden="true">2.</strong> Assessments</a></li><li class="chapter-item expanded "><a href="extensions.html"><strong aria-hidden="true">3.</strong> Extensions and Materials</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Readings</li><li class="chapter-item expanded "><a href="readings.html"><strong aria-hidden="true">4.</strong> Course Readings</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Weekly Content</li><li class="chapter-item expanded "><a href="week-01.html"><strong aria-hidden="true">5.</strong> Week 1: How to Ask a Question in Psychological Science?</a></li><li class="chapter-item expanded "><a href="week-02.html"><strong aria-hidden="true">6.</strong> Week 2: Causal Diagrams: Elementary Structures</a></li><li class="chapter-item expanded "><a href="week-03.html"><strong aria-hidden="true">7.</strong> Week 3: Causal Diagrams: Confounding Bias</a></li><li class="chapter-item expanded "><a href="week-04.html"><strong aria-hidden="true">8.</strong> Week 4: Selection Bias and Measurement Bias</a></li><li class="chapter-item expanded "><a href="week-05.html"><strong aria-hidden="true">9.</strong> Week 5: Average Treatment Effects</a></li><li class="chapter-item expanded "><a href="week-06.html"><strong aria-hidden="true">10.</strong> Week 6: Effect Modification / CATE</a></li><li class="chapter-item expanded "><a href="week-07.html"><strong aria-hidden="true">11.</strong> Week 7: In-Class Test 1</a></li><li class="chapter-item expanded "><a href="week-08.html"><strong aria-hidden="true">12.</strong> Week 8: Heterogeneous Treatment Effects and Machine Learning</a></li><li class="chapter-item expanded "><a href="week-09.html"><strong aria-hidden="true">13.</strong> Week 9: Resource Allocation and Policy Trees</a></li><li class="chapter-item expanded "><a href="week-10.html"><strong aria-hidden="true">14.</strong> Week 10: Classical Measurement Theory from a Causal Perspective</a></li><li class="chapter-item expanded "><a href="week-11.html"><strong aria-hidden="true">15.</strong> Week 11: In-Class Test 2</a></li><li class="chapter-item expanded "><a href="week-12.html"><strong aria-hidden="true">16.</strong> Week 12: Student Presentations</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Weekly Labs</li><li class="chapter-item expanded "><a href="lab-01.html"><strong aria-hidden="true">17.</strong> Lab 1: Git and GitHub</a></li><li class="chapter-item expanded "><a href="lab-02.html"><strong aria-hidden="true">18.</strong> Lab 2: Install R and Set Up Your IDE</a></li><li class="chapter-item expanded "><a href="lab-03.html"><strong aria-hidden="true">19.</strong> Lab 3: Regression, Graphing, and Simulation</a></li><li class="chapter-item expanded "><a href="lab-04.html"><strong aria-hidden="true">20.</strong> Lab 4: Regression and Confounding Bias</a></li><li class="chapter-item expanded "><a href="lab-05.html"><strong aria-hidden="true">21.</strong> Lab 5: Average Treatment Effects</a></li><li class="chapter-item expanded "><a href="lab-06.html"><strong aria-hidden="true">22.</strong> Lab 6: CATE and Effect Modification</a></li><li class="chapter-item expanded "><a href="lab-08.html"><strong aria-hidden="true">23.</strong> Lab 8: RATE and QINI Curves</a></li><li class="chapter-item expanded "><a href="lab-09.html"><strong aria-hidden="true">24.</strong> Lab 9: Policy Trees</a></li><li class="chapter-item expanded "><a href="lab-10.html"><strong aria-hidden="true">25.</strong> Lab 10: Measurement Invariance</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Resources: Test Preparation Study</li><li class="chapter-item expanded "><a href="test-1-study-sheet.html"><strong aria-hidden="true">26.</strong> Test 1 Study Sheet</a></li><li class="chapter-item expanded "><a href="test-1-quiz-2024.html"><strong aria-hidden="true">27.</strong> Practice Quiz 2024 (with Answers)</a></li><li class="chapter-item expanded "><a href="test-1-quiz-2025.html"><strong aria-hidden="true">28.</strong> Practice Test 2025 (Test 1)</a></li><li class="chapter-item expanded "><a href="test-1-quiz-2025-answers.html"><strong aria-hidden="true">29.</strong> Practice Test 2025 Answers</a></li><li class="chapter-item expanded "><a href="test-2-study-sheet.html"><strong aria-hidden="true">30.</strong> Test 2 Study Sheet</a></li><li class="chapter-item expanded "><a href="test-2-study-prompts.html"><strong aria-hidden="true">31.</strong> Test 2 Self-Study Prompts</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Resources: Understanding Causal Inference</li><li class="chapter-item expanded "><a href="potential-outcomes.html"><strong aria-hidden="true">32.</strong> Potential Outcomes and Causal Inference</a></li><li class="chapter-item expanded "><a href="causal-workflow.html"><strong aria-hidden="true">33.</strong> The Causal Workflow: Ten Steps</a></li><li class="chapter-item expanded "><a href="reporting-guide.html"><strong aria-hidden="true">34.</strong> Reporting Guide</a></li><li class="chapter-item expanded "><a href="presentation-rubric.html"><strong aria-hidden="true">35.</strong> Presentation Rubric</a></li><li class="chapter-item expanded "><a href="simulation-guide.html"><strong aria-hidden="true">36.</strong> Simulation Guide</a></li><li class="chapter-item expanded "><a href="glossary.html"><strong aria-hidden="true">37.</strong> Glossary and DAG Hand-outs</a></li><li class="chapter-item expanded affix "><li class="spacer"></li><li class="chapter-item expanded affix "><li class="part-title">Extras</li><li class="chapter-item expanded "><a href="ggdag-tutorial.html"><strong aria-hidden="true">38.</strong> Causal Diagrams with ggdag</a></li><li class="chapter-item expanded "><a href="vim-motions-with-zed.html"><strong aria-hidden="true">39.</strong> Vim Motions with Zed: 2-Week Starter Plan</a></li><li class="chapter-item expanded "><a href="zed-download.html"><strong aria-hidden="true">40.</strong> Zed Download and Install</a></li><li class="chapter-item expanded "><a href="exercise-answers.html"><strong aria-hidden="true">41.</strong> Suggested Answers: Pair Exercises</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
