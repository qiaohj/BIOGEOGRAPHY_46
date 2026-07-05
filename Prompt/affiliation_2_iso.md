"Analyze the provided data containing two columns: `article_DOI` (Article DOI) and `affiliation` (Affiliation Information of the article). Determine the countries of each affiliation was conducted based *solely* on the content of the `affiliation` column.

Return the result as a two-column CSV file.

**Column 1:** The `article_DOI` of the article.
**Column 2:** The country code(s) corresponding to the affiliation. Use the **ISO 3166-1 alpha-3 code standard**. 

**Constraints:**

1. **One record per DOI:** Each `article_DOI` must appear only once in the output file.
2. **Filtering:** If the country cannot be reliably determined from the `affiliation`, **discard that entire record**.
3. **DOI Integrity:** Every `affiliation` in the output file **must** exist in the input data. Do not generate or include any DOIs that are not in the provided list."