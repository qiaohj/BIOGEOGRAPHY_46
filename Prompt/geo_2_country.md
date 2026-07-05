"Analyze the provided data containing two columns: `doi` (Article DOI) and `scope_name` (Location Information within the article). Determine the countries where the research of each article (`doi`) was conducted based *solely* on the content of the `scope_name` column.

Return the result as a two-column CSV file.

**Column 1:** The `doi` of the article.
**Column 2:** The country code(s) corresponding to the research location(s). Use the **ISO 3166-1 alpha-3 code standard**. If an article covers multiple countries, separate the country codes using a pipe symbol (`|`).

**Constraints:**

1. **One record per DOI:** Each `doi` must appear only once in the output file.
2. **Multiple Countries:** Ensure multiple country codes for a single DOI are strictly separated by the pipe symbol (`|`) (e.g., `USA|CAN|MEX`).
3. **Filtering:** If the country cannot be reliably determined from the `scope_name`, **discard that entire record**.
4. **DOI Integrity:** Every `doi` in the output file **must** exist in the input data. Do not generate or include any DOIs that are not in the provided list."